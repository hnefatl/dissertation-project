{-# Language BangPatterns, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TupleSections #-}

module ExtraDefs where

import Data.List
import Data.Foldable
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M

allM, anyM :: (Foldable f, Monad m) => (a -> m Bool) -> f a -> m Bool
allM f = foldlM (\x y -> (x &&) <$> f y) True
anyM f = foldlM (\x y -> (x ||) <$> f y) False

ifM, whenM :: Monad m => m Bool -> m () -> m ()
ifM p action = do
    trigger <- p
    when trigger action
whenM = ifM

ifJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
ifJustM p action = p >>= maybe (return ()) action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p action = do
    trigger <- p
    unless trigger action

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p xs = fmap fst . find snd <$>  mapM (\x -> (x,) <$> p x) xs

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


reverseLookup :: Ord v => v -> M.Map k v -> Maybe k
reverseLookup x = fmap fst . find ((x ==) . snd) . M.toList