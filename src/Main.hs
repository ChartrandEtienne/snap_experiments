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
import qualified Snap.Snaplet.Session as Sess
import qualified Control.Lens as Lens
import qualified Snap.Snaplet as Snaplet
import qualified Snap.Snaplet.Session.Backends.CookieSession as CookieSession
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Snap as S
-- import qualified Snap.Snaplet.Persistent as SnapletPersistent
import Snap.Util.FileServe (serveDirectory, serveFile)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text

import qualified Data.Aeson as Aeson

import Data.Aeson ((.=))

import qualified Crypto.PasswordStore as PWord

import qualified Control.Monad.State as MonadState
import qualified Control.Monad.Trans.Reader as MonadReader
import qualified Control.Monad.Logger as MonadLogger
import qualified Control.Monad.Trans.Resource as MonadResource
import qualified Snap.Snaplet.PostgresqlSimple as PSql
import qualified Database.PostgreSQL.Simple.Time as PSQLTime
import Database.PostgreSQL.Simple.Types 
import Database.PostgreSQL.Simple.ToField

data App = App 
    { _sess ::  S.Snaplet Sess.SessionManager
    , _db   ::  S.Snaplet PSql.Postgres
    }

Lens.makeLenses ''App

data Comic = Comic { c_id :: Int, c_url_prefix :: Text.Text } deriving (Show)

instance PSql.FromRow Comic where
    fromRow = Comic CApp.<$> PSql.field CApp.<*> PSql.field

data Visit = Visit { v_id :: Int, v_url :: Text.Text, v_datetime :: PSQLTime.LocalTimestamp, v_user_id :: Int } deriving (Show)

instance PSql.FromRow Visit where 
    fromRow = Visit CApp.<$> PSql.field CApp.<*> PSql.field CApp.<*> PSql.field CApp.<*> PSql.field

instance Aeson.ToJSON Visit where
    toJSON (Visit id url datetime usr_id) = Aeson.object ["url" .= url, "timestamp" .= datetime] 

instance Aeson.ToJSON PSQLTime.LocalTimestamp where
    toJSON (PSQLTime.Finite ts) = Aeson.String $ Text.pack $ show ts
    toJSON _ = Aeson.object ["fuck" .= ("rekt" :: String)]

data InsertVisit = InsertVisit { iv_url :: Text.Text, iv_usr_id :: Int } deriving (Show)

data GetInsertVisit = GetInsertVisit { giv_url :: Text.Text } deriving (Show)

makeInsertVisit id iv = InsertVisit (giv_url iv) id

instance Aeson.FromJSON GetInsertVisit where
    parseJSON (Aeson.Object v) = 
        GetInsertVisit CApp.<$> v Aeson..: "url"
    parseJSON _ = mzero

instance PSql.ToRow InsertVisit where
    toRow p = [toField $ iv_url p, toField $ iv_usr_id p]

data LinkInput = LinkInput { url :: Text.Text, title :: Text.Text } deriving (Show)

data Usr = Usr { usr_id :: Int, usr_login :: String, usr_password :: String, usr_apikey :: String } deriving (Show)

data Post = Post { post_id :: Maybe Int, post_url :: Text.Text, post_title :: Text.Text, post_usr_fk :: Int } deriving (Show)

instance Aeson.ToJSON Post where
    toJSON (Post post_id post_url post_title post_usr_fk) = Aeson.object ["url" .= post_url, "title" .= post_title]

instance PSql.FromRow Usr where
    fromRow = Usr CApp.<$> PSql.field CApp.<*> PSql.field CApp.<*> PSql.field CApp.<*> PSql.field

instance PSql.FromRow Post where
    fromRow = Post CApp.<$> PSql.field CApp.<*> PSql.field CApp.<*> PSql.field CApp.<*> PSql.field 

instance PSql.ToRow Post where
    toRow p = [toField $ post_url p, toField $ post_title p, toField $ post_usr_fk p]

instance Aeson.FromJSON LinkInput where
    parseJSON (Aeson.Object v)  = 
        LinkInput   CApp.<$> v Aeson..: "url" 
                    CApp.<*> v Aeson..: "title"
    parseJSON _ = mzero

-- userForm = User
--     <$> "name" .: text Nothing
--     <*> "mail" .: check "Not a valid email address" checkEmail (text Nothing)

type MyHandler a = S.Handler App App a

someRoute :: MyHandler ()
someRoute = do
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
        , "</html>"  
        , "<script src='/frontend.js'></script>"
        ]

maybe_user :: Text.Text -> MyHandler (Maybe Usr)
maybe_user name = do
    onePossibleUser <- S.with db $ PSql.query "select * from login where apikey = ?" (PSql.Only name) :: MyHandler [Usr]
    case onePossibleUser of 
        [Usr id login password apikey] -> return $ Just $ Usr id login password apikey
        _ -> return Nothing

authHandler :: MyHandler ()
authHandler = do

    maybe_login <- S.getPostParam "login"
    maybe_password <- S.getPostParam "password"

    case (fmap decodeUtf8 maybe_login, maybe_password) of
        (Just login, Just given_password) -> do
            onePossibleUser <- S.with db $ PSql.query "select * from login where login = ?" (PSql.Only login :: PSql.Only Text.Text) :: MyHandler [Usr]
            case onePossibleUser of 
                [Usr id login password apikey] -> do
                    case PWord.verifyPassword given_password (pack password) of 
                        True -> S.with sess $ Sess.setInSession "user" (Text.pack apikey) >> Sess.commitSession >> return ()
                        False -> return ()
                _ -> return ()
        _ -> S.writeBS "not okay"

    {- 
    let login = decodeUtf8 $ fromMaybe "" maybe_login
    onePossibleUser <- S.with db $ PSql.query "select * from login where login = ?" (PSql.Only login :: PSql.Only Text.Text) :: MyHandler [Usr]
    case onePossibleUser of 
        [Usr id login password apikey] -> S.with sess $ Sess.setInSession "user" (Text.pack login) >> Sess.commitSession
        _ -> return ()
    -}
    S.redirect "/"

registerHandler :: MyHandler ()
registerHandler = do
    maybe_login <- S.getPostParam "login"
    maybe_password <- S.getPostParam "password"
    case (fmap decodeUtf8 maybe_login, maybe_password) of
        (Just login, Just password) -> do
            hopefullyNobody <- S.with db $ PSql.query "select * from login where login = ?" (PSql.Only login :: PSql.Only Text.Text) :: MyHandler [Usr]
            case hopefullyNobody of 
                [] -> do
                    paword <- liftIO $ PWord.makePassword password 10
                    S.with db $ PSql.execute "insert into login (login, password) values (?, ?)" (login, paword)
                    S.redirect "/"
                _ -> S.writeBS "SOMEBODY EXISTS"
        _ -> S.writeBS "I NEED USERNAME PAWORD"

    
    
headerAuth :: (Usr -> MyHandler ()) -> MyHandler ()
headerAuth success = do
    req <- S.getRequest
    let header = S.getHeader "userid" req
    case header of 
        Just userid -> do
            maybe_usr <- maybe_user $ decodeUtf8 userid
            -- let maybe_usr =  Just "ught"
            case maybe_usr of
                Just usr -> success $ usr
                _ -> S.writeBS "nope here"
        Nothing -> S.writeBS "nope"

cookieAuth :: (Usr -> MyHandler ()) -> MyHandler ()
cookieAuth success = do
    maybe_sess <- S.with sess $ Sess.getFromSession "user"
    case maybe_sess of 
        Just sess -> do
            maybe_usr <- maybe_user sess
            case maybe_usr of
                Just usr -> success $ usr
                _ -> S.writeBS "nope here"
        _ -> S.writeBS "nope there"

searchVisitsHandler :: Usr -> MyHandler()
searchVisitsHandler user = S.method S.GET $ do
    murl <- S.getParam "url"
    case murl of 
        Just url -> do 
            comics <- S.with db $ PSql.query "select * from comic where strpos(?, url_prefix) != 0" (PSql.Only murl) :: MyHandler [Comic]
            case comics of 
                [comic] -> do
                    visits <- S.with db $ PSql.query "select * from visit where strpos(url, ?) != 0" (PSql.Only $ c_url_prefix comic) :: MyHandler [Visit]

                    S.modifyResponse $ S.setHeader "Content-Type" "application/json"
                    S.writeBS $ toStrict $ Aeson.encode visits
                _ -> S.writeBS "none part deux"
        Nothing -> S.writeBS "none"

headerHandler :: Usr -> MyHandler() 
headerHandler user = S.writeBS "YES"

pinsHandler :: Usr -> MyHandler () 
pinsHandler user = S.method S.GET $ do
    posts <- S.with db $ PSql.query "select * from post where usr_id = ?" (PSql.Only $ usr_id user) :: MyHandler [Post]
    let to_string = toStrict $ Aeson.encode posts
    -- S.writeBS $ ByteString.concat ["so yeah; ", "fuck persistent: ", to_string]
    S.modifyResponse $ S.setHeader "Content-Type" "application/json"
    S.writeBS to_string
    -- S.writeBS "yeah"

addVisitHandler :: Usr -> MyHandler ()
addVisitHandler user = S.method S.POST $ do
    uh <- Aeson.decode `fmap` S.readRequestBody 100000 :: MyHandler (Maybe GetInsertVisit)
    case uh of 
        Just (GetInsertVisit url) -> do
            let insert_visit = InsertVisit url (usr_id user)
            S.with db $ PSql.execute "insert into visit (url, usr_id) values (?, ?)" insert_visit
            return ()
        Nothing -> return ()
    S.writeBS "erm"

addPinHandler :: Usr -> MyHandler ()
addPinHandler user = S.method S.POST $ do
    uh <- Aeson.decode `fmap` S.readRequestBody 100000 :: MyHandler (Maybe LinkInput)
    S.modifyResponse $ S.setHeader "Content-Type" "application/json"
    case uh of 
        Just (LinkInput url title) -> do
            let post = Post Nothing url title (usr_id user)
            S.with db $ PSql.execute "insert into post (url, title, usr_id) values (?, ?, ?)" post
            -- S.writeBS "uh"
            return ()
        Nothing ->
            -- S.writeBS "erm"
            return ()
            
    S.writeBS $ ByteString.concat ["okay", pack $ show uh]

routes = 
    [ ("/", someRoute)
    , ("/login", authHandler)
    , ("/register", registerHandler)
    , ("/pins/add", cookieAuth addPinHandler)
    , ("/visit/search", cookieAuth searchVisitsHandler)
    , ("/visit/add", headerAuth addVisitHandler)
    , ("/visit/header", headerAuth headerHandler)
    , ("/pins", cookieAuth pinsHandler)
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

{- 
main = do
    let uh = Aeson.decode "{\"url\": \"yo\"}" :: Maybe LinkInput
    putStrLn $ show uh
    return ()
-}

