{-# LANGUAGE FlexibleContexts #-}

module Optimisations.TopLevelDedupe where

import           BasicPrelude
import           Control.Monad.Except (MonadError, throwError)
import           Data.Bifunctor       (bimap)
import qualified Data.HashMap.Strict  as HM
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import           TextShow             (showt)

import           Backend.ILA          (Alt(..), AltConstructor(..), Binding(..))
import           Backend.ILB
import           Logger               (MonadLogger, writeLog)
import           NameGenerator        (MonadNameGenerator, freshVarName)
import           Names                (VariableName(..))


performTopLevelDedupe :: (MonadLogger m, MonadError Text m, MonadNameGenerator m) => [Binding Rhs] -> m [Binding Rhs]
performTopLevelDedupe bs = do
    m <- getHashedBindings bs
    writeLog $ showt m
    -- Get a new set of bindings with duplicate bindings deduplicated and renamed throughout
    (bs', renamings) <- fmap unzip $ forM (HM.toList m) $ \(r, vs) -> case S.toList vs of
        [v] -> return (NonRec v r, []) -- Unique binding, don't rename
        _ -> do
            v <- freshVarName
            return (NonRec v r, zip (S.toList vs) (repeat v))
    let renamingMap = M.fromList $ concat renamings
    writeLog $ showt renamingMap
    return $ map (renameBinding renamingMap) bs'

getHashedBindings :: MonadError Text m => [Binding Rhs] -> m (HM.HashMap Rhs (S.Set VariableName))
getHashedBindings = fmap (foldl' (HM.unionWith S.union) HM.empty) . mapM getHashedBinding

getHashedBinding :: MonadError Text m => Binding Rhs -> m (HM.HashMap Rhs (S.Set VariableName))
getHashedBinding (NonRec v rhs) = return $ HM.singleton rhs (S.singleton v)
getHashedBinding Rec{}          = throwError "Recursive bindings not supported in TopLevelDedupe"


renameVar :: M.Map VariableName VariableName -> VariableName -> VariableName
renameVar ren v = M.findWithDefault v v ren

renameArg :: M.Map VariableName VariableName -> Arg -> Arg
renameArg _ l@ArgLit{}   = l
renameArg ren (ArgVar v) = ArgVar (renameVar ren v)

renameExp :: M.Map VariableName VariableName -> Exp -> Exp
renameExp _ l@ExpLit{}            = l
renameExp ren (ExpVar v)          = ExpVar (renameVar ren v)
renameExp ren (ExpApp f as)       = ExpApp (renameVar ren f) (map (renameArg ren) as)
renameExp ren (ExpConApp c as)    = ExpConApp c (map (renameArg ren) as)
renameExp ren (ExpCase e t vs as) = ExpCase (renameExp ren e) t (map (renameVar ren) vs) (map (renameAlt ren) as)
renameExp ren (ExpLet v r e)      = ExpLet (renameVar ren v) (renameRhs ren r) (renameExp ren e)

renameRhs :: M.Map VariableName VariableName -> Rhs -> Rhs
renameRhs ren (RhsClosure vs e) = RhsClosure (map (renameVar ren) vs) (renameExp ren e)

renameAlt :: M.Map VariableName VariableName -> Alt Exp -> Alt Exp
renameAlt ren (Alt c e) = Alt (renameAltConstructor ren c) (renameExp ren e)

renameAltConstructor :: M.Map VariableName VariableName -> AltConstructor -> AltConstructor
renameAltConstructor ren (DataCon c vs) = DataCon c (map (renameVar ren) vs)
renameAltConstructor _ Default          = Default

renameBinding :: M.Map VariableName VariableName -> Binding Rhs -> Binding Rhs
renameBinding ren (NonRec v r) = NonRec (renameVar ren v) (renameRhs ren r)
renameBinding ren (Rec m)      = Rec $ M.fromList $ map (bimap (renameVar ren) (renameRhs ren)) $ M.toList m
