{-# Language MultiParamTypeClasses, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Names where

import Language.Haskell.Syntax as Syntax
import Data.Hashable

newtype VariableName = VariableName String deriving (Eq, Ord, Hashable)
newtype UniqueVariableName = UniqueVariableName String deriving (Eq, Ord, Hashable)
newtype TypeVariableName = TypeVariableName String deriving (Eq, Ord, Hashable)
newtype TypeConstantName = TypeConstantName String deriving (Eq, Ord, Hashable)
instance Show VariableName where
    show (VariableName s) = s
instance Show UniqueVariableName where
    show (UniqueVariableName s) = s
instance Show TypeVariableName where
    show (TypeVariableName s) = s
instance Show TypeConstantName where
    show (TypeConstantName s) = s

class NameConvertible n1 n2 where
    convertName :: n1 -> n2

instance NameConvertible Syntax.HsName String where
    convertName (HsIdent name) = name
    convertName (HsSymbol name) = name
instance NameConvertible Syntax.HsQName String where
    convertName (Qual _ name) = convertName name
    convertName (UnQual name) = convertName name
    convertName (Special HsUnitCon) = "()"
    convertName (Special HsListCon) = "[]"
    convertName (Special HsCons) = ":"
    convertName (Special HsFunCon) = "->"
    convertName (Special (HsTupleCon _)) = "(,)" -- TODO(kc506): Could support (,,) syntax etc...
instance NameConvertible Syntax.HsName TypeVariableName where
    convertName name = TypeVariableName (convertName name)
instance NameConvertible Syntax.HsQName TypeVariableName where
    convertName name = TypeVariableName (convertName name)
instance NameConvertible Syntax.HsName TypeConstantName where
    convertName name = TypeConstantName (convertName name)
instance NameConvertible Syntax.HsQName TypeConstantName where
    convertName name = TypeConstantName (convertName name)
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