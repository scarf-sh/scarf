{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Main where

import           Lib
import qualified Models                                   as DB

import           Control.Monad.Reader                     (MonadReader, ReaderT,
                                                           ask, runReaderT)
import           Crypto.BCrypt
import           Crypto.JOSE.JWK
import           Data.Aeson                               (FromJSON, ToJSON)
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.ByteArray.Encoding
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Char8                    as B8S
import           Data.Maybe
import           Data.Pool
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           Data.Text.Encoding
import           Data.Time.Clock
import           Database.Beam
import qualified Database.Beam.Backend.SQL.BeamExtensions as Extensions
import           Database.Beam.Postgres
import           Lens.Micro.Platform
import           Network.Wai.Handler.Warp                 (run)
import           Servant
import           Servant.Auth.Server
import           Servant.HTML.Blaze                       (HTML)
import           Text.Blaze.Html5                         (Html)
import qualified Text.Blaze.Html5                         as BZ
import           Text.Printf

connString = "postgres://avipress@127.0.0.1:5432/udb?sslmode=disable"

data State = State { connPool :: Pool Connection, jwtKey :: JWK }
type AppM = ReaderT State Handler

data EmptyOk = EmptyOk

deriveJSON defaultOptions ''EmptyOk

data CreateUserRequest = CreateUserRequest
  { createUserRequestEmail    :: Text
  , createUserRequestUsername :: Text
  , createUserRequestPassword :: Text
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CreateUserRequest"}
  ''CreateUserRequest
makeFields ''CreateUserRequest

data LoginRequest = LoginRequest
  { loginRequestEmail    :: Text
  , loginRequestPassword :: Text
  }

deriveJSON
  defaultOptions {fieldLabelModifier = makeFieldLabelModfier "loginRequest"}
  ''LoginRequest
makeFields ''LoginRequest

data Session = Session {
  sessionUserId   :: Integer,
  sessionEmail    :: Text,
  sessionUsername :: Text
  } deriving (Generic, Show)

deriveJSON
  defaultOptions {fieldLabelModifier = makeFieldLabelModfier "session"}
  ''Session
deriving instance ToJWT Session
deriving instance FromJWT Session
makeFields ''Session

data CreatePackageCallRequest = CreatePackageCallRequest
  { createPackageCallRequestPackageId :: Integer
  , createPackageCallRequestExit      :: Integer
  , createPackageCallRequestRunTimeMs :: Integer
  , createPackageCallRequestArgString :: Text
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CreatePackageCallRequest"}
  ''CreatePackageCallRequest
makeFields ''CreatePackageCallRequest

type UAPI = "static" :> Raw

            :<|> "user" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)

            :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)

            :<|> CaptureAll "anything-else" Text :> Get '[HTML] Html

type ProtectedAPI = "logged-in" :> Get '[JSON] Session

type OptionallyProtectedAPI = "package-call" :> ReqBody '[JSON] CreatePackageCallRequest :> Post '[JSON] NoContent

type FullAPI auths = (Auth auths Session :> ProtectedAPI) :<|> (Auth auths Session :> OptionallyProtectedAPI) :<|> UAPI

uAPI :: Proxy UAPI
uAPI = Proxy

fullAPI :: Proxy (FullAPI '[Cookie, JWT])
fullAPI = Proxy

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

hoistServerWithAuth
  :: HasServer api '[CookieSettings, JWTSettings]
  => Proxy api
  -> (forall x. m x -> n x)
  -> ServerT api m
  -> ServerT api n
hoistServerWithAuth api =
  hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings])

app :: Servant.Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> State -> Application
app cfg cs jwts s =
  serveWithContext fullAPI cfg $
  hoistServerWithContext
    fullAPI
    (Proxy :: Proxy '[ CookieSettings, JWTSettings])
    (nt s)
    (fullServer cs jwts)

unprotected :: CookieSettings -> JWTSettings -> ServerT UAPI AppM
unprotected cs jwts =
  serveDirectoryFileServer "./web/dist"
  :<|> (createUser cs jwts)
  :<|> (login cs jwts)
  :<|> rootHandler

protected :: AuthResult Session -> ServerT ProtectedAPI AppM
protected (Authenticated s) = isLoggedInHandler s
protected _                 = throwAll err401

optionallyProtected :: AuthResult Session -> ServerT OptionallyProtectedAPI AppM
optionallyProtected (Authenticated s) = createPackageCallHandler (Just s)
optionallyProtected  Indefinite       = createPackageCallHandler Nothing
optionallyProtected  _                = throwAll err401

fullServer :: CookieSettings -> JWTSettings -> ServerT (FullAPI auths) AppM
fullServer cs jwts = protected :<|> optionallyProtected :<|> unprotected cs jwts

createUser ::
     CookieSettings
  -> JWTSettings
  -> CreateUserRequest
  -> AppM ((Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent))
createUser cSettings jSettings req = do
  (State pool _) <- ask
  token <- liftIO DB.genApiToken
  pwHash <-
    liftIO $
    hashPasswordUsingPolicy slowerBcryptHashingPolicy . encodeUtf8 $
    req ^. password
  case pwHash of
    Nothing -> fail "error processing password"
    hash -> do
      newUserList <-
        liftIO $
        withResource pool $ \conn ->
          runBeamPostgres conn $ do
            inserted <- Extensions.runInsertReturningList (DB._repoUsers DB.repoDb) $
              insertExpressions
                [ DB.User
                    default_
                    (val_ $ req ^. email)
                    (val_ $ req ^. username)
                    (val_ $ maybe "" decodeUtf8 hash)
                    (val_ Nothing)
                    (val_ token)
                    default_
                    (val_ Nothing)
                ]
            return inserted
      applyCookieResult <-
        liftIO $
        acceptLogin
          cSettings
          jSettings
          -- FIXME(#borked) figure out the types here so we're not calling `head`!
          (Session ((head newUserList) ^. DB.id) (req ^. email) (req ^. username))
      case applyCookieResult of
        Nothing         -> fail "couldn't apply cookie"
        (Just applyRes) -> return $ applyRes NoContent

createPackageCallHandler :: Maybe Session -> CreatePackageCallRequest -> AppM NoContent
createPackageCallHandler maybeSession req = do
  let callerUserId = (^. userId) <$> maybeSession
  (State pool _) <- ask
  _ <-
    liftIO $
    withResource pool $ \conn ->
      runBeamPostgresDebug putStrLn conn $ do
        runInsert $ insert (DB._repoPackageCalls DB.repoDb) $
          insertExpressions
            [ DB.PackageCall
            default_
            (val_ $ DB.PackageId $ req ^. packageId)
            (val_ $ DB.UserId callerUserId)
            (val_ $ req ^. exit)
            (val_ $ req ^. runTimeMs)
            (val_ $ req ^. argString)
            (default_)
            ]
        return ()
  return NoContent

isLoggedInHandler :: Session -> AppM Session
isLoggedInHandler = return

login ::
     CookieSettings
  -> JWTSettings
  -> LoginRequest
  -> AppM ((Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent))
login c j r = do
  (State pool jwk) <- ask
  let (reqEmail :: Text) = r ^. email
  result <-
    liftIO $
    withResource pool $ \conn ->
      runBeamPostgresDebug putStrLn conn $ do
        matchingUser <-
          runSelectReturningOne $
          select
            (filter_
               (\u -> (u ^. DB.email) ==. val_ reqEmail)
               (all_ (DB._repoUsers DB.repoDb)))
        case matchingUser of
          Nothing -> do
            liftIO . putStrLn $ printf "%s not found" reqEmail
            return $ Left "Incorrect details"
          (Just user) ->
            if validatePassword
                 (encodeUtf8 $ user ^. DB.password)
                 (encodeUtf8 $ r ^. password)
              then return $ Right user
              else return $ Left "Your credentials are invalid."
  case result of
    Left err -> do
      liftIO $ putStrLn err
      throwError $ err401 {errBody="incorrect details"}
    Right user -> do
      applyCookieResult <- liftIO $ acceptLogin c j (Session (user ^. DB.id) (reqEmail) $ user ^. DB.username)
      case applyCookieResult of
        Nothing         -> fail "counld't apply cookie"
        (Just applyRes) -> return $ applyRes NoContent

rootHandler :: [Text] -> AppM Html
rootHandler path =
  if null path
    then liftIO $ BZ.preEscapedToHtml <$> readFile "./web/dist/index.html"
    else throwError $ err404 {errBody = "Not found"}

main = do
  let port = 9001
  connPool <- DB.initConnectionPool connString
  now <- getCurrentTime
  jwkKey <- readKey "./jwk.json"
  let cookieSettings = defaultCookieSettings{cookieIsSecure=NotSecure, cookiePath=(Just "*")}
  let jwtSettings = defaultJWTSettings jwkKey
  let contextConfig = cookieSettings :. jwtSettings :. EmptyContext
  putStrLn $ "serving on " <> show port
  run 9001 $ app contextConfig cookieSettings jwtSettings $ State connPool jwkKey
