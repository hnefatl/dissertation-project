{-# Language MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Names where

import Language.Haskell.Syntax as Syntax
import Data.Hashable

import AlphaEq

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

instance NameConvertible Syntax.HsName TypeVariableName where
    convertName (HsIdent name) = TypeVariableName name
    convertName (HsSymbol name) = TypeVariableName name
instance NameConvertible Syntax.HsQName TypeVariableName where
    convertName (Qual _ name) = convertName name
    convertName (UnQual name) = convertName name
    convertName (Special _) = error "No support for special constructors"
instance NameConvertible Syntax.HsName TypeConstantName where
    convertName (HsIdent name) = TypeConstantName name
    convertName (HsSymbol name) = TypeConstantName name
instance NameConvertible Syntax.HsQName TypeConstantName where
    convertName (Qual _ name) = convertName name
    convertName (UnQual name) = convertName name
    convertName (Special _) = error "No support for special constructors"
instance NameConvertible Syntax.HsName VariableName where
    convertName (HsIdent name) = VariableName name
    convertName (HsSymbol name) = VariableName name
instance NameConvertible Syntax.HsQName VariableName where
    convertName (Qual _ name) = convertName name
    convertName (UnQual name) = convertName name
    convertName (Special _) = error "No support for special constructors"
instance NameConvertible Syntax.HsOp VariableName where
    convertName (HsVarOp name) = convertName name
    convertName (HsConOp name) = convertName name
instance NameConvertible Syntax.HsQOp VariableName where
    convertName (HsQVarOp name) = convertName name
    convertName (HsQConOp name) = convertName name

instance AlphaEq TypeVariableName where
    alphaEq' (TypeVariableName s1) (TypeVariableName s2) = alphaEq' s1 s2