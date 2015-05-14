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
import qualified Control.Applicative as CApp

-- import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Database.Persist as Persist
import qualified Database.Persist.Sqlite as Sqlite
import qualified Database.Persist.TH as TH
import qualified Snap.Snaplet.Session as Sess
import qualified Control.Lens as Lens
import qualified Snap.Snaplet as Snaplet
import qualified Snap.Snaplet.Session.Backends.CookieSession as CookieSession
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as ByteString
import qualified Snap as S
-- import qualified Snap.Snaplet.Persistent as SnapletPersistent
import Snap.Util.FileServe (serveDirectory, serveFile)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified MyDatabase as MyDatabase

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))

import qualified Control.Monad.State as MonadState
import qualified Control.Monad.Trans.Reader as MonadReader
import qualified Control.Monad.Logger as MonadLogger
import qualified Control.Monad.Trans.Resource as MonadResource
import qualified Snap.Snaplet.PostgresqlSimple as PSql

data App = App 
    { _sess ::  S.Snaplet Sess.SessionManager
    , _db   ::  S.Snaplet PSql.Postgres
    }

Lens.makeLenses ''App

data LinkInput = LinkInput { url :: String }

data Usr = Usr { usr_id :: Int, usr_login :: String } deriving (Show)

data Post = Post { post_id :: Int, post_url :: Text.Text, post_title :: Text.Text, post_usr_fk :: Int } deriving (Show)

instance Aeson.ToJSON Post where
    toJSON (Post post_id post_url post_title post_usr_fk) = Aeson.object ["url" .= post_url, "title" .= post_title]

instance PSql.FromRow Usr where
    fromRow = Usr CApp.<$> PSql.field CApp.<*> PSql.field

instance PSql.FromRow Post where
    fromRow = Post CApp.<$> PSql.field CApp.<*> PSql.field CApp.<*> PSql.field CApp.<*> PSql.field

type MyHandler a = S.Handler App App a

someRoute :: MyHandler ()
someRoute = do
    posts <- S.with db $ PSql.query_ "select 2 + 2" 
    let urgh = posts :: [PSql.Only Int]
    let wtf = pack $ foldl (\accu (PSql.Only foo) -> accu ++ ", " ++ (show foo)) "" urgh :: ByteString
    maybe_sess <- S.with sess $ Sess.getFromSession "user"
    let sess = fromMaybe "" maybe_sess
    S.writeBS $ ByteString.concat 
        [ "<head>"
        , "<script>window.user = '", (encodeUtf8 sess), "'</script>"
        , "</head>"
        , "<html>"
        , "<div id='react_hook'></div>"
        , "<p>", wtf, "</p>"
        , "</html>"  
        , "<script src='/frontend.js'></script>"
        ]

-- maybe_user :: Text.Text -> MyHandler (Maybe (Sqlite.Entity Usr))
maybe_user name = do
    -- onePossibleUser <- MyDatabase.runPersist $ Persist.selectList [UsrLogin Persist.==. (Text.unpack name)] [Persist.LimitTo 1] 
    -- let onePossibleUser = undefined 
    -- case onePossibleUser of
    --             [Sqlite.Entity uh ah]   -> return $ Just $ Sqlite.Entity uh ah
    --             _   -> return Nothing

    onePossibleUser <- S.with db $ PSql.query_ "select * from usr" :: MyHandler [Usr]
    case onePossibleUser of 
        [Usr id login] -> return $ Just $ Usr id login
        _ -> return Nothing

authHandler :: MyHandler ()
authHandler = do
    maybe_name <- S.getPostParam "user"
    let name = decodeUtf8 $ fromMaybe "" maybe_name
    onePossibleUser <- S.with db $ PSql.query_ "select * from usr" :: MyHandler [(Int, Text.Text)]
    case onePossibleUser of 
        [(id, login)] -> S.with sess $ Sess.setInSession "user" login >> Sess.commitSession
        _ -> return ()
    S.redirect "/"
    

auth :: (Usr -> MyHandler ()) -> MyHandler ()
auth success = do
    maybe_sess <- S.with sess $ Sess.getFromSession "user"
    case maybe_sess of 
        Just sess -> do
            maybe_usr <- maybe_user sess
            case maybe_usr of
                Just usr -> success $ usr
                _ -> S.writeBS "nope here"
        _ -> S.writeBS "nope there"


pinsHandler :: Usr -> MyHandler () 
pinsHandler user = S.method S.GET $ do
    posts <- S.with db $ PSql.query "select * from post where usr_id = ?" (PSql.Only $ usr_id user) :: MyHandler [Post]
    let to_string = toStrict $ Aeson.encode posts
    S.writeBS $ ByteString.concat ["so yeah; ", "fuck persistent: ", to_string]

addPinHandler :: Usr -> MyHandler ()
addPinHandler user = S.method S.POST $ do
    S.writeBS "okay"

routes = 
    [ ("/", someRoute)
    , ("/login", authHandler)
    , ("/pins/add", auth addPinHandler)
    , ("/pins", auth pinsHandler)
    , ("/posts/add", auth addPinHandler) 
    , ("/frontend.js", serveFile "./frontend/dist/app.js")
    ]

app :: Snaplet.SnapletInit App App
app = Snaplet.makeSnaplet "app" "yeah" Nothing $ do
    s <- Snaplet.nestSnaplet "sess" sess $
        CookieSession.initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    S.addRoutes routes
    d <- S.nestSnaplet "db" db PSql.pgsInit
    return $ App s d

main = Snaplet.serveSnaplet S.defaultConfig app

