{-# Language GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, LambdaCase #-}

module AlphaEq where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import ExtraDefs

newtype AlphaEqM a = AlphaEqM { inner :: State (M.Map String String) a }
    deriving (Functor, Applicative, Monad, MonadState (M.Map String String))

class AlphaEq a where
    alphaEq' :: a -> a -> AlphaEqM Bool

alphaEq :: AlphaEq a => a -> a -> Bool
alphaEq x y = evalState (inner $ alphaEq' x y) M.empty

runAlphaEq :: AlphaEq a => a -> a -> (Bool, M.Map String String)
runAlphaEq x y = runState (inner $ alphaEq' x y) M.empty

instance AlphaEq String where
    alphaEq' s1 s2
        | s1 == s2 = return True
        | otherwise = (,) <$> gets (M.lookup s1) <*> gets (M.lookup s2) >>= \case
            -- Neither's been renamed before, add renames to them both
            (Nothing, Nothing) -> do
                modify (M.union $ M.fromList [(s1, s2), (s2, s1)])
                return True
            -- Both have been renamed before, check if they've been renamed to each other
            (Just x, Just y) -> return $ x == s2 && y == s1
            -- One's been renamed but the other hasn't: they can't be renamed to the same thing
            (_, _) -> return False
instance (Ord a, AlphaEq a) => AlphaEq (S.Set a) where
    alphaEq' s1 s2
        | S.null s1 && S.null s2 = return True -- Both empty, both alpha equivalent
        | S.null s1 || S.null s2 = return False -- Different sizes, can't be alpha equivalent
        | otherwise = do -- Find an element from a set that's alpha-eq to one from the other set, remove it, recurse
            let x = S.findMin s1 -- Arbitrary element from first set
            findM (alphaEq' x) (S.toList s2) >>= \case -- Find an alpha-eq element from the other set
                Nothing -> return False -- No matching element
                Just y -> alphaEq' (S.delete x s1) (S.delete y s2) -- Found an equivalent element, remove and recurse