{-# Language MultiParamTypeClasses, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Names where

import BasicPrelude
import Data.Text (unpack, pack)
import TextShow (TextShow, showb, fromText)
import Language.Haskell.Syntax as Syntax

newtype VariableName = VariableName Text deriving (Eq, Ord, Hashable)
newtype TypeVariableName = TypeVariableName Text deriving (Eq, Ord, Hashable)
newtype TypeConstantName = TypeConstantName Text deriving (Eq, Ord, Hashable)
-- Unique variants are only used within the renamer
newtype UniqueVariableName = UniqueVariableName Text deriving (Eq, Ord, Hashable)
newtype UniqueTypeVariableName = UniqueTypeVariableName Text deriving (Eq, Ord, Hashable)
instance TextShow VariableName where
    showb (VariableName s) = fromText s
instance TextShow TypeVariableName where
    showb (TypeVariableName s) = fromText s
instance TextShow UniqueVariableName where
    showb (UniqueVariableName s) = fromText s
instance TextShow UniqueTypeVariableName where
    showb (UniqueTypeVariableName s) = fromText s
instance Show VariableName where
    show (VariableName s) = unpack s
instance Show TypeVariableName where
    show (TypeVariableName s) = unpack s
instance Show UniqueVariableName where
    show (UniqueVariableName s) = unpack s
instance Show UniqueTypeVariableName where
    show (UniqueTypeVariableName s) = unpack s


class NameConvertible n1 n2 where
    convertName :: n1 -> n2

instance NameConvertible Syntax.HsName Text where
    convertName (HsIdent name) = pack name
    convertName (HsSymbol name) = pack name
instance NameConvertible Syntax.HsQName Text where
    convertName (Qual _ name) = convertName name
    convertName (UnQual name) = convertName name
    convertName (Special HsUnitCon) = "()"
    convertName (Special HsListCon) = "[]"
    convertName (Special HsCons) = ":"
    convertName (Special HsFunCon) = "->"
    convertName (Special (HsTupleCon _)) = "(,)" -- TODO(kc506): Could support (,,) syntax etc...
instance NameConvertible VariableName Text where
    convertName (VariableName n) = n
instance NameConvertible TypeVariableName Text where
    convertName (TypeVariableName n) = n
instance NameConvertible UniqueVariableName Text where
    convertName (UniqueVariableName n) = n
instance NameConvertible UniqueTypeVariableName Text where
    convertName (UniqueTypeVariableName n) = n
instance NameConvertible Syntax.HsName TypeVariableName where
    convertName name = TypeVariableName (convertName name)
instance NameConvertible Syntax.HsQName TypeVariableName where
    convertName name = TypeVariableName (convertName name)
instance NameConvertible Syntax.HsName VariableName where
    convertName name = VariableName (convertName name)
instance NameConvertible Syntax.HsQName VariableName where
    convertName name = VariableName (convertName name)
instance NameConvertible Syntax.HsOp VariableName where
    convertName (HsVarOp name) = convertName name
    convertName (HsConOp name) = convertName name
instance NameConvertible Syntax.HsQOp VariableName where
    convertName (HsQVarOp name) = convertName name
    convertName (HsQConOp name) = convertName name