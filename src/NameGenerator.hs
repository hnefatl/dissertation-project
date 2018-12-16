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
    -- |Get/set the counter value (shouldn't really be used, only used for lifting a specific instance to the class)
    getCounter :: m NameGeneratorCounter
    setCounter :: NameGeneratorCounter -> m ()

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
    getCounter = NameGeneratorT get
    setCounter = NameGeneratorT . put
freshVarName :: MonadNameGenerator m => m VariableName
freshVarName = VariableName . ('v':) <$> freshName
freshUniqueVarName :: MonadNameGenerator m => m UniqueVariableName
freshUniqueVarName = UniqueVariableName . ('v':) <$> freshName
freshTypeVarName :: MonadNameGenerator m => m TypeVariableName
freshTypeVarName = TypeVariableName . ('t':) <$> freshName

liftNameGenerator :: MonadNameGenerator m => NameGenerator a -> m a
liftNameGenerator x = do
    i <- getCounter
    let (y, i') = runNameGenerator x i
    setCounter i'
    return y

instance MonadNameGenerator m => MonadNameGenerator (ExceptT e m) where
    freshName = lift freshName
    setCounter = lift . setCounter
    getCounter = lift getCounter
instance MonadNameGenerator m => MonadNameGenerator (ReaderT e m) where
    freshName = lift freshName
    setCounter = lift . setCounter
    getCounter = lift getCounter
instance MonadNameGenerator m => MonadNameGenerator (StateT e m) where
    freshName = lift freshName
    setCounter = lift . setCounter
    getCounter = lift getCounter
instance MonadNameGenerator m => MonadNameGenerator (IdentityT m) where
    freshName = lift freshName
    setCounter = lift . setCounter
    getCounter = lift getCounter

instance MonadError e m => MonadError e (NameGeneratorT m) where
    throwError = lift . throwError
    catchError (NameGeneratorT x) f = NameGeneratorT $ catchError x $ (\(NameGeneratorT y) -> y) . f
instance MonadState s m => MonadState s (NameGeneratorT m) where
    state = lift . state
instance MonadReader s m => MonadReader s (NameGeneratorT m) where
    ask = lift ask
    local f (NameGeneratorT x) = NameGeneratorT $ local f x