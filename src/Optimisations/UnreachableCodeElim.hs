{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Optimisations.UnreachableCodeElim where

import           BasicPrelude
import           Control.Monad.Reader (MonadReader, asks, runReader)
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S

import           Backend.ILA          (Alt(..), Binding(..))
import           Backend.ILB
import           Names                (VariableName)

-- To find unreachable procedures, start at main and find every used variable, then every variable used by those
-- variables, etc. When done, remove all bindings that aren't used.


elimUnreachableCode :: VariableName -> [Binding Rhs] -> [Binding Rhs]
elimUnreachableCode mainName bs = runReader (catMaybes <$> mapM removeBindingUnusedVariables bs) usedVariables
    where usedVariables = getUsedVariables (S.singleton mainName) bs S.empty


getUsedVariables :: S.Set VariableName -> [Binding Rhs] -> S.Set VariableName -> S.Set VariableName
getUsedVariables vars bindings visited = case S.minView vars of
    Nothing -> visited
    Just (v, vs) -> case partition (S.member v . bindingBoundNames) bindings of
        -- Used variable must be let-bound within a top-level binding or already processed: just carry on
        ([], _)   -> getUsedVariables vs bindings visited'
        -- Get the used variables from the variables within this binding, merge them
        ([b], bs) -> getUsedVariables (S.union (S.difference (getBindingUsedVariables b) visited') vs) bs visited'
        _         -> error "Duplicate bindings in UnreachableCodeElim"
      where visited' = S.insert v visited

bindingBoundNames :: Binding a -> S.Set VariableName
bindingBoundNames (NonRec v _) = S.singleton v
bindingBoundNames (Rec m)      = M.keysSet m


getBindingUsedVariables :: Binding Rhs -> S.Set VariableName
getBindingUsedVariables (NonRec _ r) = getRhsUsedVariables r
getBindingUsedVariables (Rec m)      = S.unions $ map getRhsUsedVariables $ M.elems m

getRhsUsedVariables :: Rhs -> S.Set VariableName
getRhsUsedVariables (RhsClosure _ e) = getExpUsedVariables e

getExpUsedVariables :: Exp -> S.Set VariableName
getExpUsedVariables ExpLit{}         = S.empty
getExpUsedVariables (ExpVar v)       = S.singleton v
getExpUsedVariables (ExpApp v as)    = S.insert v (S.unions $ map getArgUsedVariables as)
getExpUsedVariables (ExpConApp _ as) = S.unions $ map getArgUsedVariables as
getExpUsedVariables (ExpCase s _ as) = S.unions $ getExpUsedVariables s:map getAltUsedVariables as
getExpUsedVariables (ExpLet _ rhs e) = S.union (getRhsUsedVariables rhs) (getExpUsedVariables e)

getAltUsedVariables :: Alt Exp -> S.Set VariableName
getAltUsedVariables (Alt _ e) = getExpUsedVariables e

getArgUsedVariables :: Arg -> S.Set VariableName
getArgUsedVariables ArgLit{}   = S.empty
getArgUsedVariables (ArgVar v) = S.singleton v


removeBindingUnusedVariables :: MonadReader (S.Set VariableName) m => Binding Rhs -> m (Maybe (Binding Rhs))
removeBindingUnusedVariables (NonRec v r) = do
    used <- asks (S.member v)
    r' <- removeRhsUnusedVariables r
    return $ if used then Just (NonRec v r') else Nothing
removeBindingUnusedVariables (Rec m) = do
    let nonrecs = [ NonRec v r | (v, r) <- M.toList m ]
    catMaybes <$> mapM removeBindingUnusedVariables nonrecs >>= \case
        [] -> return Nothing
        bs -> return $ Just $ Rec $ M.fromList [ (v, r) | NonRec v r <- bs ]

removeRhsUnusedVariables :: MonadReader (S.Set VariableName) m => Rhs -> m Rhs
removeRhsUnusedVariables (RhsClosure vs e) = RhsClosure vs <$> removeExpUnusedVariables e

removeExpUnusedVariables :: MonadReader (S.Set VariableName) m => Exp -> m Exp
removeExpUnusedVariables l@ExpLit{} = return l
removeExpUnusedVariables v@ExpVar{} = return v
removeExpUnusedVariables a@ExpApp{} = return a
removeExpUnusedVariables a@ExpConApp{} = return a
removeExpUnusedVariables (ExpCase s t as) = ExpCase <$> removeExpUnusedVariables s <*> pure t <*> mapM removeAltUnusedVariables as
removeExpUnusedVariables (ExpLet v rhs e) = do
    used <- asks (S.member v)
    e' <- removeExpUnusedVariables e
    return $ if used then ExpLet v rhs e' else e'

removeAltUnusedVariables :: MonadReader (S.Set VariableName) m => Alt Exp -> m (Alt Exp)
removeAltUnusedVariables (Alt c e) = Alt c <$> removeExpUnusedVariables e
