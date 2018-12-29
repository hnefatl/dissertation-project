{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Logger (MonadLogger, writeLogs, writeLog, getLogs, clearLogs, LoggerT, evalLoggerT, runLoggerT, runStateT, Logger, evalLogger, runLogger) where

import BasicPrelude
import Control.Monad.Except       (ExceptT, MonadError, catchError, throwError)
import Control.Monad.Identity     (Identity, IdentityT, runIdentity)
import Control.Monad.Reader       (MonadReader, ReaderT, ask, local)
import Control.Monad.State.Strict (MonadState, StateT, get, modify, put, runStateT, state)
import Control.Monad.Trans        (MonadTrans)
import Data.Foldable              (toList)
import Data.Sequence              (Seq, fromList)
import Data.Text                  (Text)

class Monad m => MonadLogger m where
    writeLogs :: [Text] -> m ()
    getLogs :: m [Text]
    clearLogs :: m ()

writeLog :: MonadLogger m => Text -> m ()
writeLog l = writeLogs [l]

newtype LoggerT m a = LoggerT (StateT (Seq Text) m a)
    deriving (Functor, Applicative, Monad, MonadTrans)
type Logger a = LoggerT Identity a

evalLoggerT :: Monad m => LoggerT m a -> m a
evalLoggerT = fmap fst . runLoggerT
runLoggerT :: Monad m => LoggerT m a -> m (a, [Text])
runLoggerT (LoggerT x) = fmap (fmap toList) (runStateT x mempty)
evalLogger :: Logger a -> a
evalLogger = fst . runLogger
runLogger :: Logger a -> (a, [Text])
runLogger = runIdentity . runLoggerT

instance Monad m => MonadLogger (LoggerT m) where
    writeLogs l = LoggerT $ modify (<> fromList l)
    getLogs = LoggerT (toList <$> get)
    clearLogs = LoggerT $ put mempty

instance MonadLogger m => MonadLogger (ExceptT e m) where
    writeLogs = lift . writeLogs
    getLogs = lift getLogs
    clearLogs = lift clearLogs
instance MonadLogger m => MonadLogger (ReaderT e m) where
    writeLogs = lift . writeLogs
    getLogs = lift getLogs
    clearLogs = lift clearLogs
instance MonadLogger m => MonadLogger (StateT e m) where
    writeLogs = lift . writeLogs
    getLogs = lift getLogs
    clearLogs = lift clearLogs
instance MonadLogger m => MonadLogger (IdentityT m) where
    writeLogs = lift . writeLogs
    getLogs = lift getLogs
    clearLogs = lift clearLogs

instance MonadError e m => MonadError e (LoggerT m) where
    throwError = lift . throwError
    catchError (LoggerT x) f = LoggerT $ catchError x $ (\(LoggerT y) -> y) . f
instance MonadState s m => MonadState s (LoggerT m) where
    state = lift . state
instance MonadReader s m => MonadReader s (LoggerT m) where
    ask = lift ask
    local f (LoggerT x) = LoggerT $ local f x
instance MonadIO m => MonadIO (LoggerT m) where
    liftIO = lift . liftIO
