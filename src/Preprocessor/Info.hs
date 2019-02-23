{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Preprocessor.Info where

import           BasicPrelude
import qualified Data.Map.Strict         as M
import           Control.Monad.Except    (MonadError, throwError)
import           Language.Haskell.Syntax
import           Data.Foldable           (foldlM)
import           TextShow                (TextShow, showb, showt)
import           TextShowHsSrc           ()
import           Typechecker.Types       (Kind(..))
import           Names                   (TypeVariableName, convertName)

data ClassInfo = ClassInfo
    { methods      :: M.Map HsName HsQualType
    , argVariables :: [(TypeVariableName, Kind)] }
    deriving (Eq, Show)
instance TextShow ClassInfo where
    showb = fromString . show

getClassInfo :: MonadError Text m => HsModule -> m (M.Map HsQName ClassInfo)
getClassInfo (HsModule _ _ _ _ decls) = M.unions <$> mapM getDeclClassInfo decls

getDeclClassInfo :: MonadError Text m => HsDecl -> m (M.Map HsQName ClassInfo)
getDeclClassInfo (HsClassDecl _ _ name args ds) = do
    let methodTypes = M.unions $ map getDeclTypes ds
    argKinds <- mapM (\a -> (convertName a,) <$> getArgKindFromTypes (M.elems methodTypes) a) args
    let ci = ClassInfo { methods = methodTypes, argVariables = argKinds }
    return $ M.singleton (UnQual name) ci
getDeclClassInfo _ = return M.empty

getDeclTypes :: HsDecl -> M.Map HsName HsQualType
getDeclTypes (HsTypeSig _ names t) = M.fromList $ zip names (repeat t)
getDeclTypes _                     = M.empty

getArgKindFromTypes :: MonadError Text m => [HsQualType] -> HsName -> m Kind
getArgKindFromTypes ts name = do
    mks <- mapM (\(HsQualType _ t) -> getArgKindFromType name t) ts
    mk <- foldlM (mergeKinds name) Nothing mks
    case mk of
        Nothing -> throwError $ "Failed to find kind for variable " <> showt name
        Just k -> return k

getArgKindFromType :: MonadError Text m => HsName -> HsType -> m (Maybe Kind)
getArgKindFromType name (HsTyVar v)
    | v == name = return (Just KindStar)
    | otherwise = return Nothing
getArgKindFromType _ HsTyCon{} = return Nothing
getArgKindFromType name (HsTyFun t1 t2) = do
    mk1 <- getArgKindFromType name t1
    mk2 <- getArgKindFromType name t2
    mergeKinds name mk1 mk2
getArgKindFromType name (HsTyTuple ts) = do
    mks <- mapM (getArgKindFromType name) ts
    foldlM (mergeKinds name) Nothing mks
getArgKindFromType name (HsTyApp t1 t2) = do
    mk1 <- getArgKindFromType name t1
    mk2 <- getArgKindFromType name t2
    case (mk1, mk2) of
        (Nothing, Nothing) -> return Nothing
        -- TODO(kc506): Should've be KindStar, it should be the kind of t2
        (Just k, Nothing) -> return $ Just $ KindFun KindStar k
        (Nothing, Just k) -> return (Just k)
        (Just _, Just _) -> throwError "Application of variable to self"

mergeKinds :: MonadError Text m => HsName -> Maybe Kind -> Maybe Kind -> m (Maybe Kind)
mergeKinds _ Nothing mk = return mk
mergeKinds _ mk Nothing = return mk
mergeKinds name (Just k1) (Just k2)
    | k1 == k2 = return (Just k1)
    | otherwise = throwError $ unwords ["Mismatched kinds for ", showt name <> ":", showt k1, showt k2]


getModuleKinds :: HsModule -> M.Map TypeVariableName Kind
getModuleKinds (HsModule _ _ _ _ decls) = getDeclsKinds decls

getDeclsKinds :: [HsDecl] -> M.Map TypeVariableName Kind
getDeclsKinds = M.unions . map getDeclKinds

getDeclKinds :: HsDecl -> M.Map TypeVariableName Kind
getDeclKinds (HsDataDecl _ _ name args _ _) = M.singleton (convertName name) typeKind
    where typeKind = foldr KindFun KindStar $ replicate (length args) KindStar
getDeclKinds _ = M.empty
    