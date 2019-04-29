{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

module Preprocessor.Info where

import           BasicPrelude
import           Control.DeepSeq         (NFData)
import           Control.Monad.Except    (MonadError, throwError)
import           Data.Foldable           (foldlM)
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import           GHC.Generics            (Generic)
import           Language.Haskell.Syntax
import           Names                   (TypeVariableName, convertName)
import           TextShow                (TextShow, showb)
import           Typechecker.Types       (Kind(..), unmakeSynApp)

data ClassInfo = ClassInfo
    { methods      :: M.Map HsName HsQualType
    , argVariables :: [(HsName, Kind)] }
    deriving (Eq, Show, Generic)
instance NFData ClassInfo
instance TextShow ClassInfo where
    showb = fromString . show

getClassInfo :: MonadError Text m => HsModule -> m (M.Map HsQName ClassInfo)
getClassInfo (HsModule _ _ _ _ decls) = M.unions <$> mapM getDeclClassInfo decls

getDeclClassInfo :: MonadError Text m => HsDecl -> m (M.Map HsQName ClassInfo)
getDeclClassInfo (HsClassDecl _ _ name args ds) = do
    argKinds <- foldlM (\m d -> disjointKindUnion m =<< getClassArgKinds d) M.empty ds
    argVars <- forM args $ \arg -> case M.lookup arg argKinds of
        Nothing -> throwError $ "Class " <> convertName name <> " argument unused in signatures: " <> convertName arg
        Just k  -> return (arg, k)
    let methodTypes = M.unions $ map getDeclTypes ds
        ci = ClassInfo { methods = methodTypes, argVariables = argVars }
    return $ M.singleton (UnQual name) ci
getDeclClassInfo _ = return M.empty

getDeclTypes :: HsDecl -> M.Map HsName HsQualType
getDeclTypes (HsTypeSig _ names t) = M.fromList $ zip names (repeat t)
getDeclTypes _                     = M.empty

getClassArgKinds :: MonadError Text m => HsDecl -> m (M.Map HsName Kind)
getClassArgKinds (HsTypeSig _ _ (HsQualType _ t)) = getTypeArgKinds t
getClassArgKinds _                                = return M.empty

getTypeArgKinds :: MonadError Text m => HsType -> m (M.Map HsName Kind)
getTypeArgKinds (HsTyVar v) = return $ M.singleton v KindStar
getTypeArgKinds t@HsTyApp{} = case unmakeSynApp t of
    -- This doesn't allow for higher-order kind arguments like `(* -> *) -> *`, that would require unification
    (HsTyVar v, args) -> return $ M.singleton v varKind
        where varKind = foldr KindFun KindStar $ replicate (length args) KindStar
    _ -> return M.empty
getTypeArgKinds (HsTyFun t1 t2) = do
    ks1 <- getTypeArgKinds t1
    ks2 <- getTypeArgKinds t2
    disjointKindUnion ks1 ks2
getTypeArgKinds _ = return M.empty

disjointKindUnion :: MonadError Text m => M.Map HsName Kind -> M.Map HsName Kind -> m (M.Map HsName Kind)
disjointKindUnion ks1 ks2 = do
    let inter = S.intersection (M.keysSet ks1) (M.keysSet ks2)
    unless (all (\k -> M.lookup k ks1 == M.lookup k ks2) inter) $ throwError "Mismatching kinds in getTypeArgKinds"
    return $ M.union ks1 ks2

getModuleKinds :: HsModule -> M.Map TypeVariableName Kind
getModuleKinds (HsModule _ _ _ _ decls) = getDeclsKinds decls

getDeclsKinds :: [HsDecl] -> M.Map TypeVariableName Kind
getDeclsKinds = M.unions . map getDeclKinds

getDeclKinds :: HsDecl -> M.Map TypeVariableName Kind
getDeclKinds (HsDataDecl _ _ name args _ _) = M.singleton (convertName name) typeKind
    where typeKind = foldr KindFun KindStar $ replicate (length args) KindStar
getDeclKinds _ = M.empty

