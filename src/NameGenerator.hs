{-# Language MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, UndecidableInstances #-}

module NameGenerator where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Identity

import Names

type NameGeneratorCounter = Int
class Monad m => MonadNameGenerator m where
    -- |Should generate a new unique name each time it's run
    freshName :: m String

newtype NameGeneratorT m a = NameGeneratorT (StateT Int m a)
    deriving (Applicative, Functor, Monad, MonadTrans)
type NameGenerator = NameGeneratorT Identity

runNameGeneratorT :: Monad m => NameGeneratorT m a -> NameGeneratorCounter -> m (a, NameGeneratorCounter)
runNameGeneratorT (NameGeneratorT x) = runStateT x
evalNameGeneratorT :: Monad m => NameGeneratorT m a -> NameGeneratorCounter -> m a
evalNameGeneratorT x i = fst <$> runNameGeneratorT x i
runNameGenerator :: NameGenerator a -> NameGeneratorCounter -> (a, NameGeneratorCounter)
runNameGenerator x i = runIdentity (runNameGeneratorT x i)
evalNameGenerator :: NameGenerator a -> NameGeneratorCounter -> a
evalNameGenerator x i = runIdentity (evalNameGeneratorT x i)

instance Monad m => MonadNameGenerator (NameGeneratorT m) where
    freshName = NameGeneratorT $ state (\i -> (show i, i+1))
freshVarName :: MonadNameGenerator m => m VariableName
freshVarName = VariableName . ('v':) <$> freshName
freshUniqueVarName :: MonadNameGenerator m => m UniqueVariableName
freshUniqueVarName = UniqueVariableName . ('v':) <$> freshName
freshTypeVarName :: MonadNameGenerator m => m TypeVariableName
freshTypeVarName = TypeVariableName . ('t':) <$> freshName

instance MonadNameGenerator m => MonadNameGenerator (ExceptT e m) where
    freshName = lift freshName
instance MonadNameGenerator m => MonadNameGenerator (ReaderT e m) where
    freshName = lift freshName
instance MonadNameGenerator m => MonadNameGenerator (StateT e m) where
    freshName = lift freshName
instance MonadNameGenerator m => MonadNameGenerator (IdentityT m) where
    freshName = lift freshName

instance MonadError e m => MonadError e (NameGeneratorT m) where
    throwError = lift . throwError
    catchError (NameGeneratorT x) f = NameGeneratorT $ catchError x $ (\(NameGeneratorT y) -> y) . f
instance MonadState s m => MonadState s (NameGeneratorT m) where
    state = lift . state
instance MonadReader s m => MonadReader s (NameGeneratorT m) where
    ask = lift ask
    local f (NameGeneratorT x) = NameGeneratorT $ local f x