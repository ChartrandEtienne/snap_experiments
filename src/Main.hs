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
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Snap as S
import qualified Snap.Snaplet.Persistent as SnapletPersistent
import Snap.Util.FileServe (serveDirectory, serveFile)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text

data App = App 
    { _sess ::  S.Snaplet Sess.SessionManager
    , _db   ::  S.Snaplet Persistent
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

type MyHandler = S.Handler App App ()

someRoute :: MyHandler
someRoute = do
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

authHandler :: MyHandler
authHandler = do
    maybe_name <- S.getPostParam "user"
    let name = decodeUtf8 $ fromMaybe "" maybe_name
    S.with sess $ Sess.setInSession "user" name >> Sess.commitSession
    S.redirect "/"

auth :: (Text.Text -> MyHandler) -> MyHandler
auth success = do
     
    maybe_sess <- S.with sess $ Sess.getFromSession "user"
    case maybe_sess of 
        Just user -> success user
        Nothing -> S.writeBS "nope"

pinsHandler :: Text.Text -> MyHandler
pinsHandler user = do

    maybe_sess <- S.with sess $ Sess.getFromSession "user"
    S.writeBS $ encodeUtf8 $ Text.concat ["you're ", user]

-- loginRoute :: 

routes = 
    [ ("/", someRoute)
    , ("/login", authHandler)
    , ("/pins", auth pinsHandler)
    , ("/frontend.js", serveFile "./frontend/dist/app.js")
    ]

app :: Snaplet.SnapletInit App App
app = Snaplet.makeSnaplet "app" "yeah" Nothing $ do
    d <- S.nestSnaplet "db" db $ Persist.initPersist (Sqlite.runMigration migrateAll)
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
