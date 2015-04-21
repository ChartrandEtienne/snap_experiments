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

newtype PersistState = PersistState { persistPool :: Sqlite.ConnectionPool }

class MonadState.MonadIO m => HasPersistPool m where
    getPersistPool :: m Sqlite.ConnectionPool

instance HasPersistPool m => HasPersistPool (MonadLogger.NoLoggingT m) where
    getPersistPool = MonadLogger.runNoLoggingT getPersistPool

instance HasPersistPool (Snaplet.Handler b PersistState) where
    getPersistPool = MonadState.gets persistPool

instance MonadState.MonadIO m => HasPersistPool (MonadReader.ReaderT Sqlite.ConnectionPool m) where
    getPersistPool = MonadReader.ask

initPersist :: Snaplet.SnapletInit b PersistState
initPersist = Snaplet.makeSnaplet "work" "please" Nothing $ do
    pool <- MonadState.liftIO $ MonadLogger.runNoLoggingT $ Sqlite.createSqlitePool ":memory:" 1
    return $ PersistState pool
