{-# Language TemplateHaskell #-}
{-# Options_GHC -fno-warn-orphans #-}

-- |We want to be able to show `haskell-src` ASTs as `Text`, but the definition of `TextShow` and the AST nodes are in
-- different modules. Time for some orphan instances......
-- Also using TH to reduce instance boilerplate
module TextShowHsSrc where

import TextShow.TH (deriveTextShow)
import Language.Haskell.Syntax

deriveTextShow ''HsModule
deriveTextShow ''HsExportSpec
deriveTextShow ''HsImportDecl
deriveTextShow ''HsImportSpec
deriveTextShow ''HsAssoc
deriveTextShow ''HsDecl
deriveTextShow ''HsConDecl
deriveTextShow ''HsBangType
deriveTextShow ''HsMatch
deriveTextShow ''HsRhs
deriveTextShow ''HsGuardedRhs
deriveTextShow ''HsSafety
deriveTextShow ''HsQualType
deriveTextShow ''HsType
deriveTextShow ''HsExp
deriveTextShow ''HsStmt
deriveTextShow ''HsFieldUpdate
deriveTextShow ''HsAlt
deriveTextShow ''HsGuardedAlts
deriveTextShow ''HsGuardedAlt
deriveTextShow ''HsPat
deriveTextShow ''HsPatField
deriveTextShow ''HsLiteral
deriveTextShow ''Module
deriveTextShow ''HsQName
deriveTextShow ''HsName
deriveTextShow ''HsQOp
deriveTextShow ''HsOp
deriveTextShow ''HsSpecialCon
deriveTextShow ''HsCName
deriveTextShow ''SrcLoc