module Preprocessor.Info where

import           BasicPrelude
import qualified Data.Map.Strict         as M
import           Language.Haskell.Syntax
import           Names                   (TypeVariableName, convertName)
import           TextShow                (TextShow, showb)
import           Typechecker.Types       (Kind(..))

data ClassInfo = ClassInfo
    { methods      :: M.Map HsName HsQualType
    , argVariables :: [HsName] }
    deriving (Eq, Show)
instance TextShow ClassInfo where
    showb = fromString . show

getClassInfo :: HsModule -> M.Map HsQName ClassInfo
getClassInfo (HsModule _ _ _ _ decls) = M.unions $ map getDeclClassInfo decls

getDeclClassInfo :: HsDecl -> M.Map HsQName ClassInfo
getDeclClassInfo (HsClassDecl _ _ name args ds) = M.singleton (UnQual name) ci
    where ci = ClassInfo { methods = M.unions $ map getDeclTypes ds, argVariables = args }
getDeclClassInfo _ = M.empty

getDeclTypes :: HsDecl -> M.Map HsName HsQualType
getDeclTypes (HsTypeSig _ names t) = M.fromList $ zip names (repeat t)
getDeclTypes _                     = M.empty


getModuleKinds :: HsModule -> M.Map TypeVariableName Kind
getModuleKinds (HsModule _ _ _ _ decls) = getDeclsKinds decls

getDeclsKinds :: [HsDecl] -> M.Map TypeVariableName Kind
getDeclsKinds = M.unions . map getDeclKinds

getDeclKinds :: HsDecl -> M.Map TypeVariableName Kind
getDeclKinds (HsDataDecl _ _ name args _ _) = M.singleton (convertName name) typeKind
    where typeKind = foldr KindFun KindStar $ replicate (length args) KindStar
getDeclKinds _ = M.empty

