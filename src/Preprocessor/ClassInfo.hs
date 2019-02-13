module Preprocessor.ClassInfo where

import           BasicPrelude
import qualified Data.Map.Strict         as M
import           Language.Haskell.Syntax
import           TextShow                (TextShow, showb)

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
