{-# Language GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, LambdaCase #-}

module AlphaEq where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

import Names

newtype AlphaEqM k a = AlphaEqM { inner :: State (M.Map k k) a }
    deriving (Functor, Applicative, Monad, MonadState (M.Map k k))

class Ord k => AlphaEq k a | a -> k where
    alphaEq' :: a -> a -> AlphaEqM k Bool

alphaEq :: AlphaEq k a => a -> a -> Bool
alphaEq x y = evalState (inner $ alphaEq' x y) M.empty

instance AlphaEq TypeVariableName TypeVariableName where
    alphaEq' tv1 tv2
        | tv1 == tv2 = return True
        | otherwise = (,) <$> gets (M.lookup tv1) <*> gets (M.lookup tv2) >>= \case
            -- Neither's been renamed before, add renames to them both
            (Nothing, Nothing) -> do
                modify (M.union $ M.fromList [(tv1, tv2), (tv2, tv1)])
                return True
            -- Both have been renamed before, check if they've been renamed to the same thing
            (Just x, Just y) -> return $ x == y
            -- One's been renamed but the other hasn't: they can't be renamed to the same thing
            (_, _) -> return False