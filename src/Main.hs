{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Control.Monad.IO.Class  (liftIO)
import qualified Database.Persist as Persist
import qualified Database.Persist.Sqlite as Sqlite
import qualified Database.Persist.TH as TH
import qualified Snap.Snaplet.Session as Sess
import qualified Control.Lens as Lens
import qualified Snap.Snaplet as Snaplet
import qualified Snap.Snaplet.Session.Backends.CookieSession as CookieSession
import           Data.ByteString (ByteString)
import qualified Snap as S
import Snap.Util.FileServe (serveDirectory)

data App = App 
    { _sess ::  S.Snaplet Sess.SessionManager
    }

Lens.makeLenses ''App

TH.share [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"] [TH.persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

someRoute :: S.Handler App App ()
someRoute = do
    S.writeBS "erm"

routes = [("/foo", someRoute)]

app :: Snaplet.SnapletInit App App
app = Snaplet.makeSnaplet "app" "yeah" Nothing $ do
    s <- Snaplet.nestSnaplet "sess" sess $
        CookieSession.initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    S.addRoutes routes
    return $ App s

main = Snaplet.serveSnaplet S.defaultConfig app

{- 
main :: IO ()
main = do
    Sqlite.runSqlite ":memory:" $ do
        Sqlite.runMigration migrateAll   
    S.quickHttpServe site
-}

site :: S.Snap ()
site =
    S.ifTop (S.writeBS "hello world") S.<|>
    S.route [ ("foo", S.writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] S.<|>
    S.dir "static" (serveDirectory ".")

echoHandler :: S.Snap ()
echoHandler = do
    param <- S.getParam "echoparam"
    maybe (S.writeBS "must specify echo/param in URL")
          S.writeBS param
