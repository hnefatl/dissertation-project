{-# Language BangPatterns #-}

module ExtraDefs where

import Data.Foldable

import Language.Haskell.Syntax as Syntax

allM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM f = foldlM (\x y -> (x &&) <$> f y) True

anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
anyM f = foldlM (\x y -> (x ||) <$> f y) False

-- |foldlM strict in the accumulator
foldlM' :: (Foldable f, Monad m) => (a -> b -> m a) -> a -> f b -> m a
foldlM' f !e = foldlM (\(!acc) x -> f acc x) e

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

class ToId t where
    toId :: t -> String

instance ToId Syntax.HsName where
    toId (HsIdent name) = name
    toId (HsSymbol name) = name
instance ToId Syntax.HsQName where
    toId (Qual _ name) = toId name
    toId (UnQual name) = toId name
    toId (Special _) = error "Add support for special constructors"