{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module Optimisations.BindingDedupe where

import           BasicPrelude
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Reader (MonadReader, asks, local, runReaderT)
import           Data.Default         (Default, def)
import qualified Data.HashMap.Strict  as HM
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S

import           Backend.ILA          (Alt(..), AltConstructor(..), Binding(..))
import           Backend.ILB
import           Names                (VariableName(..))

type DedupeMonad m = (MonadError Text m, MonadReader DedupeState m)

data DedupeState = DedupeState
    { -- What to rename a use site of a variable to
      renamings :: M.Map VariableName VariableName,
      -- What name to use for a given RHS
      rhss      :: HM.HashMap Rhs VariableName }
instance Default DedupeState where
    def = DedupeState
        { renamings = M.empty
        , rhss = HM.empty }

withRenamings :: DedupeMonad m => (M.Map VariableName VariableName -> a -> b) -> a -> m b
withRenamings f x = f <$> asks renamings <*> pure x

doDedupe :: (MonadError Text m) => [Binding Rhs] -> m [Binding Rhs]
doDedupe bs = catMaybes <$> runReaderT (mapM performBindingDedupe bs) def

performBindingDedupe :: DedupeMonad m => Binding Rhs -> m (Maybe (Binding Rhs))
performBindingDedupe (NonRec v rhs) = fmap (NonRec v) <$> performDedupe v rhs
performBindingDedupe Rec{}          = throwError "Recursive bindings not supported in TopLevelDedupe"

performDedupes :: DedupeMonad m => [(VariableName, Rhs)] -> m [(VariableName, Rhs)]
performDedupes bs = do
    let groupedRhss = HM.fromListWith (<>) $ map (\(v,r) -> (r, [v])) bs
        -- The new renaming maps every variable name with the same RHS, to the first variable with that rhs
        renamings' = M.fromList [ (v', v) | v:vs <- HM.elems groupedRhss, v' <- vs ]
        -- The new rhs mapping maps each rhs to the first variable with that rhs
        rhss' = HM.map head groupedRhss
    -- Add the new renaming/rhs mappings then run dedupe
    local (\s -> s { renamings = M.union renamings' (renamings s), rhss = HM.union rhss' (rhss s) }) $
        forM (HM.toList groupedRhss) $ \(rhs, v:_) -> (v,) <$> dedupeRhs rhs

performDedupe :: DedupeMonad m => VariableName -> Rhs -> m (Maybe Rhs)
performDedupe v rhs = performDedupes [(v, rhs)] >>= \case
    [] -> return Nothing
    [(_, rhs')] -> return (Just rhs')
    _ -> throwError "Weird result from performDedupes"

getHashedBindings :: MonadError Text m => [Binding Rhs] -> m (HM.HashMap Rhs (S.Set VariableName))
getHashedBindings = fmap (foldl' (HM.unionWith S.union) HM.empty) . mapM getHashedBinding

getHashedBinding :: MonadError Text m => Binding Rhs -> m (HM.HashMap Rhs (S.Set VariableName))
getHashedBinding (NonRec v rhs) = return $ HM.singleton rhs (S.singleton v)
getHashedBinding Rec{}          = throwError "Recursive bindings not supported in TopLevelDedupe"


dedupeVar :: M.Map VariableName VariableName -> VariableName -> VariableName
dedupeVar ren v = M.findWithDefault v v ren

dedupeArg :: M.Map VariableName VariableName -> Arg -> Arg
dedupeArg _ l@ArgLit{}   = l
dedupeArg ren (ArgVar v) = ArgVar (dedupeVar ren v)

dedupeExp :: DedupeMonad m => Exp -> m Exp
dedupeExp l@ExpLit{}            = return l
dedupeExp (ExpVar v)          = ExpVar <$> withRenamings dedupeVar v
dedupeExp (ExpApp f as)       = ExpApp <$> withRenamings dedupeVar f <*> mapM (withRenamings dedupeArg) as
dedupeExp (ExpConApp c as)    = ExpConApp c <$> mapM (withRenamings dedupeArg) as
dedupeExp (ExpCase e t vs as) = ExpCase <$> dedupeExp e <*> pure t <*> mapM (withRenamings dedupeVar) vs <*> mapM dedupeAlt as
dedupeExp (ExpLet v rhs e)      = performDedupe v rhs >>= \case
    Nothing -> dedupeExp e
    Just rhs' -> ExpLet v rhs' <$> dedupeExp e

dedupeRhs :: DedupeMonad m => Rhs -> m Rhs
dedupeRhs (RhsClosure vs e) = RhsClosure <$> mapM (withRenamings dedupeVar) vs <*> dedupeExp e

dedupeAlt :: DedupeMonad m => Alt Exp -> m (Alt Exp)
dedupeAlt (Alt c e) = Alt <$> withRenamings dedupeAltConstructor c <*> dedupeExp e

dedupeAltConstructor :: M.Map VariableName VariableName -> AltConstructor -> AltConstructor
dedupeAltConstructor ren (DataCon c vs) = DataCon c $ map (dedupeVar ren) vs
dedupeAltConstructor _ Default          = Default
