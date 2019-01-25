{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Names where

import BasicPrelude
import Data.Text               (pack, unpack)
import Language.Haskell.Syntax as Syntax
import TextShow                (TextShow, fromText, showb)
import Tuples                  (makeTupleName)

newtype VariableName = VariableName Text deriving (Eq, Ord, Hashable)
newtype TypeVariableName = TypeVariableName Text deriving (Eq, Ord, Hashable)
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

-- Lets us use `"f"` to mean `VariableName "f"` when the OverloadedStrings extension is enabled
instance IsString VariableName where
    fromString = VariableName . pack
instance IsString TypeVariableName where
    fromString = TypeVariableName . pack

class NameConvertible n1 n2 where
    convertName :: n1 -> n2

instance NameConvertible Syntax.HsSpecialCon Text where
    convertName HsUnitCon      = "()"
    convertName HsListCon      = "[]"
    convertName HsCons         = ":"
    convertName HsFunCon       = "->"
    convertName (HsTupleCon n) = "(,)" --makeTupleName n
instance NameConvertible Syntax.HsName Text where
    convertName (HsIdent name)  = pack name
    convertName (HsSymbol name) = pack name
    convertName (HsSpecial s) = convertName s
instance NameConvertible Syntax.HsQName Text where
    convertName (Qual _ name)            = convertName name
    convertName (UnQual name)            = convertName name
    convertName (Special s)              = convertName s
instance NameConvertible Syntax.HsOp Text where
    convertName (HsVarOp v) = convertName v
    convertName (HsConOp c) = convertName c
instance NameConvertible Syntax.HsQOp Text where
    convertName (HsQVarOp v) = convertName v
    convertName (HsQConOp c) = convertName c
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

instance NameConvertible VariableName HsName where
    convertName (VariableName n) = HsIdent $ unpack n
instance NameConvertible TypeVariableName HsName where
    convertName (TypeVariableName n) = HsIdent $ unpack n