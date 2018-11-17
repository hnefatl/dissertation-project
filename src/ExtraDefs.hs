{-# Language BangPatterns, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, UndecidableInstances #-}

module ExtraDefs where

import Data.List
import Data.Foldable
import qualified Data.Set as S

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

dedupe :: Ord a => [a] -> [a]
dedupe = S.toList . S.fromList