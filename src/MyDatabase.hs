{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MyDatabase where

import qualified Database.Persist as Persist
import qualified Database.Persist.Sqlite as Sqlite
import qualified Database.Persist.TH as TH
import qualified Snap.Snaplet as Snaplet
import qualified Data.Pool as DPool

import qualified Control.Monad.State as MonadState
import qualified Control.Monad.Trans.Reader as MonadReader
import qualified Control.Monad.Logger as MonadLogger
import qualified Control.Monad.Trans.Resource as MonadResource

newtype PersistState = PersistState { persistPool :: Sqlite.ConnectionPool }

class MonadState.MonadIO m => HasPersistPool m where
    getPersistPool :: m Sqlite.ConnectionPool

instance HasPersistPool m => HasPersistPool (MonadLogger.NoLoggingT m) where
    getPersistPool = MonadLogger.runNoLoggingT getPersistPool

instance HasPersistPool (Snaplet.Handler b PersistState) where
    getPersistPool = MonadState.gets persistPool

instance MonadState.MonadIO m => HasPersistPool (MonadReader.ReaderT Sqlite.ConnectionPool m) where
    getPersistPool = MonadReader.ask

initPersist :: Sqlite.SqlPersistT (MonadLogger.NoLoggingT IO) a -> Snaplet.SnapletInit b PersistState 
initPersist migration = Snaplet.makeSnaplet "work" "please" Nothing $ do
    pool <- MonadState.liftIO $ MonadLogger.runNoLoggingT $ Sqlite.createSqlitePool "whoa.db" 1
    _ <- MonadState.liftIO $ MonadLogger.runNoLoggingT $ Sqlite.runSqlPool migration pool
    return $ PersistState pool

-- | Runs a SqlPersist action in any monad with a HasPersistPool instance.
-- runPersist :: (HasPersistPool m)
--            => Sqlite.SqlPersistT (MonadResource.ResourceT (MonadLogger.NoLoggingT IO)) b
--            -- ^ Run given Persistent action in the defined monad.
--            -> m b
runPersist action = do
    pool <- getPersistPool
    -- MonadState.liftIO . MonadLogger.runNoLoggingT . MonadResource.runResourceT $ Sqlite.runSqlPool action pool
    withPool pool action

withPool :: MonadState.MonadIO m
         => Sqlite.ConnectionPool

         -> Sqlite.SqlPersistT (MonadResource.ResourceT (MonadLogger.NoLoggingT IO)) a -> m a
withPool cp f = MonadState.liftIO . MonadLogger.runNoLoggingT . MonadResource.runResourceT $ Sqlite.runSqlPool f cp
