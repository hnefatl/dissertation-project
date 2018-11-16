{-# Language BangPatterns, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module ExtraDefs where

import Data.List
import Data.Foldable
import Data.Hashable
import qualified Data.Set as S

import Language.Haskell.Syntax as Syntax

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
instance NameConvertible Syntax.HsName VariableName where
    convertName (HsIdent name) = VariableName name
    convertName (HsSymbol name) = VariableName name
instance NameConvertible Syntax.HsQName VariableName where
    convertName (Qual _ name) = convertName name
    convertName (UnQual name) = convertName name
    convertName (Special _) = error "No support for special constructors"

class Monad m => NameGenerator m a where
    -- |Should generate a new unique name each time it's run
    freshName :: m a

allM, anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM f = foldlM (\x y -> (x &&) <$> f y) True
anyM f = foldlM (\x y -> (x ||) <$> f y) False

-- |foldlM strict in the accumulator
foldlM' :: (Foldable f, Monad m) => (a -> b -> m a) -> a -> f b -> m a
foldlM' f !e = foldlM (\(!acc) x -> f acc x) e

pairmap :: (a -> b) -> (a, a) -> (b, b)
pairmap f (x, y) = (f x, f y)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

containsDuplicates :: (Foldable f, Ord a) => f a -> Bool
containsDuplicates l = length l /= S.size (foldl' (flip S.insert) S.empty l)

deline :: String -> String
deline = intercalate " \\n " . lines