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
import Control.Applicative
import Control.Monad
import qualified Database.Persist as Persist
import qualified Database.Persist.Sqlite as Sqlite
import qualified Database.Persist.TH as TH
import qualified Snap.Snaplet.Session as Sess
import qualified Control.Lens as Lens
import qualified Snap.Snaplet as Snaplet
import qualified Snap.Snaplet.Session.Backends.CookieSession as CookieSession
import Data.ByteString (ByteString)
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

import qualified Control.Monad.State as MonadState
import qualified Control.Monad.Trans.Reader as MonadReader
import qualified Control.Monad.Logger as MonadLogger
import qualified Control.Monad.Trans.Resource as MonadResource

import qualified MyDigestive as MyDG

import qualified Text.Digestive as DG
-- import qualified Text.Digestive.Heist as HDigestive
-- import qualified Text.Digestive.Snap as SDigestive

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

data LinkInput = LinkInput { url :: Text.Text , title :: Text.Text } deriving (Show)
data LinkFaround = LinkFaround { furl :: Maybe ByteString }

linkForm :: Monad m => DG.Form Text.Text m LinkInput
linkForm = LinkInput
    <$> "url" DG..: DG.text Nothing
    <*> "title" DG..: DG.text Nothing

instance Aeson.FromJSON LinkInput where
    parseJSON (Aeson.Object v)  = 
        LinkInput   <$> v Aeson..: "url" 
                    <*> v Aeson..: "title"
    parseJSON _ = mzero

instance Aeson.ToJSON LinkInput where
    toJSON (LinkInput url title) = Aeson.object ["url" Aeson..= url, "title" Aeson..= title]

{-
instance FromJSON Coord where
    parseJSON (Object v) = Person <$>
                           v .: "name" <*>
                           v .: "age"
    parseJSON _          = mzero
-}

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

maybe_user :: Text.Text -> MyHandler (Maybe (Sqlite.Entity Usr))
maybe_user name = do
    onePossibleUser <- MyDatabase.runPersist $ Persist.selectList [UsrLogin Persist.==. (Text.unpack name)] [Persist.LimitTo 1] 
    case onePossibleUser of
                [Sqlite.Entity uh ah]   -> return $ Just $ Sqlite.Entity uh ah
                _   -> return Nothing

authHandler :: MyHandler ()
authHandler = do
    maybe_name <- S.getPostParam "user"
    let name = decodeUtf8 $ fromMaybe "" maybe_name
    onePossibleUser <- MyDatabase.runPersist $ Persist.selectList [UsrLogin Persist.==. (Text.unpack name)] [Persist.LimitTo 1] 
    case onePossibleUser of
        [Sqlite.Entity uh ah] -> S.with sess $ Sess.setInSession "user" name >> Sess.commitSession
        _ -> return ()
    S.redirect "/"
    

auth :: (Sqlite.Entity Usr -> MyHandler ()) -> MyHandler ()
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

-- curl -X POST --data {"url": "hurl", "title": "works tho"} 127.0.0.1:8000/posts/add

addPinHandler :: MyHandler ()
addPinHandler = do
    body <- S.getRequestBody
    let erm = Aeson.decode body :: Maybe LinkInput
    let mhh = show erm
    S.writeText $ Text.pack mhh
    S.writeBS "urgh"


doesntReturnSquat :: MyHandler ()
doesntReturnSquat = do
    S.writeBS "bullshit yeah "
    (view, result) <- MyDG.runForm "" linkForm
    let jsonned = case result of
            Just x  ->  Aeson.encode x
            Nothing -> "nada"
        
    let jesus = toStrict jsonned
    -- S.writeBS $ ByteString.toStrict jsonned
    S.writeBS jesus
    S.writeBS " okay"

lookMaFunctors :: MyHandler ()
lookMaFunctors = do
    erm <- fmap LinkFaround (S.getParam "perhaps")
    lul <- S.getParam "perhaps"
    S.writeBS "yeah"

pinsHandler :: Sqlite.Entity Usr -> MyHandler () 
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
    , ("/posts/add", addPinHandler) 
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

