{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances     #-}
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
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Snap as S
-- import qualified Snap.Snaplet.Persistent as SnapletPersistent
import Snap.Util.FileServe (serveDirectory, serveFile)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified MyDatabase as MyDatabase

import qualified Control.Monad.State as MonadState
import qualified Control.Monad.Trans.Reader as MonadReader
import qualified Control.Monad.Logger as MonadLogger
import qualified Control.Monad.Trans.Resource as MonadResource

data App = App 
    { _sess ::  S.Snaplet Sess.SessionManager
    , _db   ::  S.Snaplet MyDatabase.PersistState
    }

Lens.makeLenses ''App

TH.share [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"] [TH.persistLowerCase|
Usr
    login String
    deriving Show
Link
    url String
    authorId UsrId
    deriving Show
|]

type MyHandler a = S.Handler App App a

instance MyDatabase.HasPersistPool (S.Handler App App) where
    getPersistPool = S.with db MyDatabase.getPersistPool

someRoute :: MyHandler ()
someRoute = do
    -- wtf <- MyDatabase.runPersist $ Sqlite.selectList [] [] 
    -- wtf <- MyDatabase.runPersist undefined :: S.Handler App App [Sqlite.Entity Usr]
    wtf <- MyDatabase.runPersist $ Sqlite.selectList [] [] :: S.Handler App App [Sqlite.Entity Usr]
    maybe_sess <- S.with sess $ Sess.getFromSession "user"
    let sess = fromMaybe "" maybe_sess
    S.writeBS $ ByteString.concat 
        [ "<head>"
        , "<script>window.user = '", (encodeUtf8 sess), "'</script>"
        , "</head>"
        , "<html>"
        , "<div id='react_hook'></div>"
        , "</html>"  
        , "<script src='/frontend.js'></script>"
        ]

authHandler :: MyHandler ()
authHandler = do
    maybe_name <- S.getPostParam "user"
    let name = decodeUtf8 $ fromMaybe "" maybe_name
    S.with sess $ Sess.setInSession "user" name >> Sess.commitSession
    S.redirect "/"

auth :: (Text.Text -> MyHandler () ) -> MyHandler ()
auth success = do
     
    maybe_sess <- S.with sess $ Sess.getFromSession "user"
    case maybe_sess of 
        Just user -> success user
        Nothing -> S.writeBS "nope"

pinsHandler :: Text.Text -> MyHandler () 
pinsHandler user = do
    results <- MyDatabase.runPersist $ Persist.selectList [] [] :: MyHandler [Sqlite.Entity Usr]
    let len = length results
    let conv = encodeUtf8 $ Text.pack $ show len
    S.writeBS $ ByteString.concat ["so yeah; ", conv]
    -- S.writeBS "yeah"

routes = 
    [ ("/", someRoute)
    , ("/login", authHandler)
    , ("/pins", auth pinsHandler)
    , ("/frontend.js", serveFile "./frontend/dist/app.js")
    ]

app :: Snaplet.SnapletInit App App
app = Snaplet.makeSnaplet "app" "yeah" Nothing $ do
    s <- Snaplet.nestSnaplet "sess" sess $
        CookieSession.initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- Snaplet.nestSnaplet "db" db $ MyDatabase.initPersist (Sqlite.runMigration migrateAll)
    S.addRoutes routes
    return $ App s d

main = Snaplet.serveSnaplet S.defaultConfig app

{- 
erm :: IO ()
erm = do
    Sqlite.runSqlite ":memory:" $ do
        Sqlite.runMigration migrateAll   
    S.quickHttpServe undefined
-}


echoHandler :: S.Snap ()
echoHandler = do
    param <- S.getParam "echoparam"
    maybe (S.writeBS "must specify echo/param in URL")
          S.writeBS param
