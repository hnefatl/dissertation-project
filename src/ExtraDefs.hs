{-# Language BangPatterns, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module ExtraDefs where

import Data.List
import Data.Foldable
import Data.Hashable
import qualified Data.Set as S

import Language.Haskell.Syntax as Syntax

-- |General variable/type name
newtype Id = Id String deriving (Eq, Ord, Hashable)
instance Show Id where
    show (Id s) = s

allM, anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM f = foldlM (\x y -> (x &&) <$> f y) True
anyM f = foldlM (\x y -> (x ||) <$> f y) False

-- |foldlM strict in the accumulator
foldlM' :: (Foldable f, Monad m) => (a -> b -> m a) -> a -> f b -> m a
foldlM' f !e = foldlM (\(!acc) x -> f acc x) e

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

containsDuplicates :: (Foldable f, Ord a) => f a -> Bool
containsDuplicates l = length l /= S.size (foldl' (flip S.insert) S.empty l)

class ToId t where
    toId :: t -> Id

instance ToId Syntax.HsName where
    toId (HsIdent name) = Id name
    toId (HsSymbol name) = Id name
instance ToId Syntax.HsQName where
    toId (Qual _ name) = toId name
    toId (UnQual name) = toId name
    toId (Special _) = error "No support for special constructors"

class Monad m => NameGenerator m a where
    -- |Should generate a new unique name each time it's run
    freshName :: m a

deline :: String -> String
deline = intercalate " \\n " . lines
