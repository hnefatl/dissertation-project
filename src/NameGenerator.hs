{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module NameGenerator where

import BasicPrelude
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Control.Monad.State as StL
import qualified Control.Monad.State.Strict as StS
import TextShow

import JVM.Builder.Monad

import Logger
import Names

type NameGeneratorCounter = Int
class Monad m => MonadNameGenerator m where
    -- |Should generate a new unique name each time it's run
    freshName :: m Text

newtype NameGeneratorT m a = NameGeneratorT (StS.StateT Int m a)
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
    freshName = NameGeneratorT $ StS.state (\i -> (showt i, i+1))
freshVarName :: MonadNameGenerator m => m VariableName
freshVarName = VariableName . ("v" <>) <$> freshName
freshUniqueVarName :: MonadNameGenerator m => m UniqueVariableName
freshUniqueVarName = UniqueVariableName . ("v" <>) <$> freshName
freshTypeVarName :: MonadNameGenerator m => m TypeVariableName
freshTypeVarName = TypeVariableName . ("t" <>) <$> freshName
freshUniqueTypeVarName :: MonadNameGenerator m => m UniqueTypeVariableName
freshUniqueTypeVarName = UniqueTypeVariableName . ("t" <>) <$> freshName
freshDummyVarName :: MonadNameGenerator m => m VariableName
freshDummyVarName = VariableName . ("zv" <>) <$> freshName
freshDummyTypeVarName :: MonadNameGenerator m => m TypeVariableName
freshDummyTypeVarName = TypeVariableName . ("zt" <>) <$> freshName

instance MonadNameGenerator m => MonadNameGenerator (ExceptT e m) where
    freshName = lift freshName
instance MonadNameGenerator m => MonadNameGenerator (ReaderT e m) where
    freshName = lift freshName
instance MonadNameGenerator m => MonadNameGenerator (StL.StateT e m) where
    freshName = lift freshName
instance MonadNameGenerator m => MonadNameGenerator (StS.StateT e m) where
    freshName = lift freshName
instance MonadNameGenerator m => MonadNameGenerator (LoggerT m) where
    freshName = lift freshName
instance MonadNameGenerator m => MonadNameGenerator (IdentityT m) where
    freshName = lift freshName
instance MonadNameGenerator m => MonadNameGenerator (GeneratorT m) where
    freshName = lift freshName

instance MonadError e m => MonadError e (NameGeneratorT m) where
    throwError = lift . throwError
    catchError (NameGeneratorT x) f = NameGeneratorT $ catchError x $ (\(NameGeneratorT y) -> y) . f
instance StL.MonadState s m => StL.MonadState s (NameGeneratorT m) where
    state = lift . StL.state
instance MonadReader s m => MonadReader s (NameGeneratorT m) where
    ask = lift ask
    local f (NameGeneratorT x) = NameGeneratorT $ local f x
instance MonadLogger m => MonadLogger (NameGeneratorT m) where
    writeLogs = lift . writeLogs
    getLogs = lift getLogs
    clearLogs = lift clearLogs
instance MonadGenerator m => MonadGenerator (NameGeneratorT m) where
    getGState = lift getGState
    putGState = lift . putGState
    throwG = lift . throwG
instance MonadIO m => MonadIO (NameGeneratorT m) where
    liftIO = lift . liftIO
