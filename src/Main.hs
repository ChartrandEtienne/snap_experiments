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
import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Snap as S
-- import qualified Snap.Snaplet.Persistent as SnapletPersistent
import Snap.Util.FileServe (serveDirectory, serveFile)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified MyDatabase as MyDatabase

import qualified Data.Aeson as Aeson

import qualified Control.Monad.State as MonadState
import qualified Control.Monad.Trans.Reader as MonadReader
import qualified Control.Monad.Logger as MonadLogger
import qualified Control.Monad.Trans.Resource as MonadResource
import qualified Snap.Snaplet.PostgresqlSimple as PSql
-- import Text.Digestive.Blaze.Html5
-- import Text.Digestive.Happstack
-- import Text.Digestive.Util

data App = App 
    { _sess ::  S.Snaplet Sess.SessionManager
    , _db   ::  S.Snaplet PSql.Postgres
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

data LinkInput = LinkInput { url :: String }

instance Aeson.FromJSON LinkInput where
    parseJSON (Aeson.Object v)  = 
        LinkInput CApp.<$> v Aeson..: "url" 
    parseJSON _ = mzero

-- userForm = User
--     <$> "name" .: text Nothing
--     <*> "mail" .: check "Not a valid email address" checkEmail (text Nothing)

type MyHandler a = S.Handler App App a

someRoute :: MyHandler ()
someRoute = do
    -- wtf <- MyDatabase.runPersist $ Sqlite.selectList [] [] 
    -- wtf <- MyDatabase.runPersist undefined :: S.Handler App App [Sqlite.Entity Usr]
    -- wtf <- MyDatabase.runPersist $ Sqlite.selectList [] [] :: S.Handler App App [Sqlite.Entity Usr]
    posts <- S.with db $ PSql.query_ "select 2 + 2" 
    let urgh = posts :: [PSql.Only Int]
    -- let wtf = fmap (\(PSql.Only foo) -> show foo) urgh
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

    onePossibleUser <- S.with db $ PSql.query_ "select * from login" :: MyHandler [(Int, Text.Text)]
    case onePossibleUser of 
        [(id, login)] -> return $ Just (id, login)
        _ -> return Nothing

authHandler :: MyHandler ()
authHandler = do
    maybe_name <- S.getPostParam "user"
    let name = decodeUtf8 $ fromMaybe "" maybe_name
    onePossibleUser <- S.with db $ PSql.query_ "select * from login" :: MyHandler [(Int, Text.Text)]
    case onePossibleUser of 
        [(id, login)] -> S.with sess $ Sess.setInSession "user" login >> Sess.commitSession
        _ -> return ()
    -- S.writeBS $ pack $ show onePossibleUser
    -- onePossibleUser <- MyDatabase.runPersist $ Persist.selectList [UsrLogin Persist.==. (Text.unpack name)] [Persist.LimitTo 1] 
    -- case onePossibleUser of
    --     [Sqlite.Entity uh ah] -> S.with sess $ Sess.setInSession "user" name >> Sess.commitSession
    --     _ -> return ()
    S.redirect "/"
    

auth :: ((Int, Text.Text) -> MyHandler ()) -> MyHandler ()
auth success = do
    maybe_sess <- S.with sess $ Sess.getFromSession "user"
    case maybe_sess of 
        Just sess -> do
            maybe_usr <- maybe_user sess
            case maybe_usr of
                Just usr -> success $ usr
                _ -> S.writeBS "nope here"
        _ -> S.writeBS "nope there"

    S.writeBS "nope"

pinsHandler :: (Int, Text.Text) -> MyHandler () 
pinsHandler user = S.method S.GET $ do
    S.writeBS $ ByteString.concat ["so yeah; ", "fuck persistent"]
    -- S.writeBS "yeah"

addPinHandler :: (Int, Text.Text) -> MyHandler ()
addPinHandler user = S.method S.POST $ do
    S.writeBS "okay"

routes = 
    [ ("/", someRoute)
    , ("/login", authHandler)
    , ("/pins/add", auth addPinHandler)
    , ("/pins", auth pinsHandler)
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

