{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Optimisations.LetLifting where

import           BasicPrelude
import           Control.Monad.Except        (MonadError, throwError)
import           Control.Monad.Extra         (concatMapM)
import           Data.Graph                  (graphFromEdges', topSort)
import qualified Data.Set                    as S
import           Data.Tuple.Extra            (fst3)
import           TextShow                    (showt)

import           Backend.ILA                 (Alt(..), AltConstructor(..), Binding(..))
import           Backend.ILB
import           Logger                      (MonadLogger, writeLog)
import           Names                       (VariableName(..))
import           Preprocessor.ContainedNames (getFreeVariables)

type Node = ((VariableName, Rhs), VariableName, [VariableName])

type LetLifter m = (MonadError Text m, MonadLogger m)

performLetLift :: LetLifter m => [Binding Rhs] -> m [Binding Rhs]
performLetLift = concatMapM (liftLets S.empty)

liftLets :: LetLifter m => S.Set VariableName -> Binding Rhs -> m [Binding Rhs]
liftLets bound (NonRec v (RhsClosure as e)) = do
    (e', free) <- reorderLets (S.insert v bound) e
    return $ NonRec v (RhsClosure as e'):map (uncurry NonRec . fst3) free
liftLets _ Rec{}        = throwError "Recursive bindings not yet supported by let-lifting"

-- |Rearrange the let bindings within an expression to move bindings closer to the root of the tree where possible,
-- returning the updated expression along with a list of nodes that can be moved out of the expression into a higher
-- scope (in a dependency order).
reorderLets :: LetLifter m => S.Set VariableName -> Exp -> m (Exp, [Node])
reorderLets bound e = do
    (edges, base) <- getLetNodes bound e
    let (graph, getNode) = graphFromEdges' edges
        nodeOrder = map getNode $ topSort graph
        (removeable, nonRemoveable) = getRemovableLets S.empty nodeOrder
    --writeLog $ unlines [unwords ["nodeOrder", showt nodeOrder], unwords ["removeable", showt removeable], unwords ["nonRemoveable", showt nonRemoveable], ""]
    return (foldr (uncurry ExpLet . fst3) base nonRemoveable, removeable)
reorderAltLets :: LetLifter m => S.Set VariableName -> Alt Exp -> m (Alt Exp, [Node])
reorderAltLets bound (Alt c e) = first (Alt c) <$> reorderLets (S.union (getBound c) bound) e
    where getBound Default        = S.empty
          getBound (DataCon _ vs) = S.fromList vs
reorderRhsLets :: LetLifter m => S.Set VariableName -> Rhs -> m (Rhs, [Node])
reorderRhsLets bound (RhsClosure vs e) = do
    (e', ns) <- reorderLets (S.union (S.fromList vs) bound) e
    return (RhsClosure vs e', ns)

-- |Get a list of the let-binding nodes that can be reordered, as well as the base expression at the end of a
-- let-binding chain.
getLetNodes :: LetLifter m => S.Set VariableName -> Exp -> m ([Node], Exp)
getLetNodes bound (ExpCase scrut t vs as) = do
    (scrutReordered, scrutFreeBindings) <- reorderLets bound scrut
    (altsReordered, altFreeBindings) <- unzip <$> mapM (reorderAltLets bound) as
    return (scrutFreeBindings <> concat altFreeBindings, ExpCase scrutReordered t vs altsReordered)
getLetNodes bound (ExpLet v rhs inner) = do
    (ns, base) <- getLetNodes bound inner
    (rhs', freeNodes) <- reorderRhsLets bound rhs
    fvs <- getFreeVariables rhs
    return (((v, rhs'), v, S.toList fvs):ns <> freeNodes, base)
getLetNodes _ e = return ([], e)

-- |Given a topologically sorted list of nodes, return the nodes that can be moved to an outer scope and those that
-- can't.
getRemovableLets :: S.Set VariableName -> [Node] -> ([Node], [Node])
getRemovableLets _ [] = ([], [])
getRemovableLets moved ((d, v, depVars):ns)
    | null depVars' = first ((d, v, []):) $ getRemovableLets (S.insert v moved) ns
    | otherwise = second ((d, v, depVars'):) $ getRemovableLets moved ns
        where depVars' = filter (`S.notMember` moved) depVars
