{-# Language MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, UndecidableInstances #-}

module NameGenerator where

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Identity

import Names

type NameGeneratorCounter = Int
class Monad m => MonadNameGenerator n m where
    -- |Should generate a new unique name each time it's run
    freshName :: m n

newtype NameGeneratorT m a = NameGeneratorT (StateT Int m a) deriving (Applicative, Functor, Monad, MonadTrans)
type NameGenerator = NameGeneratorT Identity

runNameGeneratorT :: Monad m => NameGeneratorT m a -> NameGeneratorCounter -> m (a, NameGeneratorCounter)
runNameGeneratorT (NameGeneratorT x) = runStateT x
evalNameGeneratorT :: Monad m => NameGeneratorT m a -> NameGeneratorCounter -> m a
evalNameGeneratorT x i = fst <$> runNameGeneratorT x i
runNameGenerator :: NameGenerator a -> NameGeneratorCounter -> (a, NameGeneratorCounter)
runNameGenerator x i = runIdentity (runNameGeneratorT x i)
evalNameGenerator :: NameGenerator a -> NameGeneratorCounter -> a
evalNameGenerator x i = runIdentity (evalNameGeneratorT x i)

instance Monad m => MonadNameGenerator VariableName (NameGeneratorT m) where
    freshName = NameGeneratorT $ state (\i -> (VariableName $ "v" ++ show i, i+1))
instance Monad m => MonadNameGenerator UniqueVariableName (NameGeneratorT m) where
    freshName = NameGeneratorT $ state (\i -> (UniqueVariableName $ "v" ++ show i, i+1))
instance Monad m => MonadNameGenerator TypeVariableName (NameGeneratorT m) where
    freshName = NameGeneratorT $ state (\i -> (TypeVariableName $ "t" ++ show i, i+1))

instance MonadNameGenerator n m => MonadNameGenerator n (ExceptT e m) where
    freshName = lift freshName
instance MonadNameGenerator n m => MonadNameGenerator n (StateT e m) where
    freshName = lift freshName
instance MonadNameGenerator n m => MonadNameGenerator n (IdentityT m) where
    freshName = lift freshName

instance MonadError e m => MonadError e (NameGeneratorT m) where
    throwError = lift . throwError
    catchError (NameGeneratorT x) f = NameGeneratorT $ catchError x $ (\(NameGeneratorT y) -> y) . f
instance MonadState s m => MonadState s (NameGeneratorT m) where
    state = lift . state