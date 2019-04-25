{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Optimisations.LetLifting where

import           BasicPrelude
import           Control.Monad.Except        (MonadError, throwError)
import           Control.Monad.Extra         (concatMapM)
import           Control.Monad.Reader        (MonadReader, local, runReaderT)
import           Data.Graph                  (graphFromEdges', topSort)
import qualified Data.Set                    as S
import           Data.Tuple.Extra            (fst3)
import           TextShow                    (showt)

import           Backend.ILA                 (Alt(..), AltConstructor(..), Binding(..))
import           Backend.ILB
import           Logger                      (MonadLogger, writeLog)
import           Names                       (VariableName(..))
import           Preprocessor.ContainedNames (getFreeVariables)

-- The first element of a node is enough information to reconstruct the let-binding with a new body. The second element
-- is the variable name bound by the let-binding. The third element is all the free variables used by the rhs of the
-- let-binding: these are used to form the dependencies between nodes.
type Node = ((VariableName, Rhs), VariableName, [VariableName])

type LetLifter m = (MonadError Text m, MonadLogger m, MonadReader (S.Set VariableName) m)

performLetLift :: (MonadError Text m, MonadLogger m) => [Binding Rhs] -> m [Binding Rhs]
performLetLift bs = runReaderT (concatMapM liftLets bs) S.empty

liftLets :: LetLifter m => Binding Rhs -> m [Binding Rhs]
liftLets (NonRec v (RhsClosure as e)) = do
    (e', free) <- local (S.insert v) (reorderLets e)
    return $ NonRec v (RhsClosure as e'):map (uncurry NonRec . fst3) free
liftLets Rec{}        = throwError "Recursive bindings not yet supported by let-lifting"

-- |Rearrange the let bindings within an expression to move bindings closer to the root of the tree where possible,
-- returning the updated expression along with a list of nodes that can be moved out of the expression into a higher
-- scope (in a dependency order).
reorderLets :: LetLifter m => Exp -> m (Exp, [Node])
reorderLets e = do
    (edges, base) <- getLetNodes e
    let (graph, getNode) = graphFromEdges' edges
        nodeOrder = reverse $ map getNode $ topSort graph
        (removeable, nonRemoveable) = getRemovableLets S.empty nodeOrder
    writeLog $ unlines [unwords ["nodeOrder", showt nodeOrder], unwords ["removeable", showt removeable], unwords ["nonRemoveable", showt nonRemoveable], ""]
    return (foldr (uncurry ExpLet . fst3) base nonRemoveable, removeable)
reorderAltLets :: LetLifter m => Alt Exp -> m (Alt Exp, [Node])
reorderAltLets (Alt c e) = first (Alt c) <$> local (S.union (getBound c)) (reorderLets e)
    where getBound Default        = S.empty
          getBound (DataCon _ vs) = S.fromList vs
reorderRhsLets :: LetLifter m => Rhs -> m (Rhs, [Node])
reorderRhsLets (RhsClosure vs e) = do
    (e', ns) <- local (S.union (S.fromList vs)) (reorderLets e)
    return (RhsClosure vs e', ns)

-- |Get a list of the let-binding nodes that can be reordered, as well as the base expression at the end of a
-- let-binding chain.
getLetNodes :: LetLifter m => Exp -> m ([Node], Exp)
getLetNodes (ExpCase scrut t vs as) = do
    (scrutReordered, scrutFreeBindings) <- reorderLets scrut
    (altsReordered, altFreeBindings) <- unzip <$> mapM reorderAltLets as
    return (scrutFreeBindings <> concat altFreeBindings, ExpCase scrutReordered t vs altsReordered)
getLetNodes (ExpLet v rhs inner) = do
    (ns, base) <- getLetNodes inner
    (rhs', freeNodes) <- reorderRhsLets rhs
    fvs <- getFreeVariables rhs
    return (((v, rhs'), v, S.toList fvs):ns <> freeNodes, base)
getLetNodes e = return ([], e)

-- |Given a topologically sorted list of nodes, return the nodes that can be moved to an outer scope and those that
-- can't.
getRemovableLets :: S.Set VariableName -> [Node] -> ([Node], [Node])
getRemovableLets _ [] = ([], [])
getRemovableLets moved ((d, v, depVars):ns)
    | null depVars' = first ((d, v, []):) $ getRemovableLets (S.insert v moved) ns
    | otherwise = second ((d, v, depVars'):) $ getRemovableLets moved ns
        where depVars' = filter (`S.notMember` moved) depVars
