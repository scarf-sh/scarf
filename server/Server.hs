{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}

module Main where

import           Lib
import qualified Models                   as DB

import           Control.Lens
import           Control.Monad.Reader     (ReaderT, ask, runReaderT)
import           Crypto.BCrypt
import           Crypto.JOSE.JWK
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.ByteArray.Encoding
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as B8S
import           Data.Maybe
import           Data.Pool
import           Data.Text                (Text)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Database.Beam
import           Database.Beam.Postgres
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.Auth.Server
import           Servant.HTML.Blaze       (HTML)
import           Text.Blaze.Html5         (Html)
import qualified Text.Blaze.Html5         as BZ
import           Text.Printf

connString = "postgres://avipress@127.0.0.1:5432/udb?sslmode=disable"

data State = State { connPool :: Pool Connection, jwtKey :: JWK }
type AppM = ReaderT State Handler

data EmptyOk = EmptyOk

deriveJSON defaultOptions ''EmptyOk

data CreateUserRequest = CreateUserRequest
  { _createUserRequestEmail    :: Text
  , _createUserRequestUsername :: Text
  , _createUserRequestPassword :: Text
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "createUserRequest"}
  ''CreateUserRequest
makeFields ''CreateUserRequest

data LoginRequest = LoginRequest
  { _loginRequestEmail    :: Text
  , _loginRequestPassword :: Text
  }

deriveJSON
  defaultOptions {fieldLabelModifier = makeFieldLabelModfier "loginRequest"}
  ''LoginRequest
makeFields ''LoginRequest

data Session = Session {
  _sessionEmail    :: Text,
  _sessionUsername :: Text
  } deriving (Generic)

deriveJSON
  defaultOptions {fieldLabelModifier = makeFieldLabelModfier "session"}
  ''Session
deriving instance ToJWT Session
deriving instance FromJWT Session
makeFields ''Session

type UAPI = "static" :> Raw
            :<|> "user" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] EmptyOk
            :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)
            :<|> CaptureAll "anything-else" Text :> Get '[HTML] Html

uAPI :: Proxy UAPI
uAPI = Proxy

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve uAPI $ hoistServer uAPI (nt s) server

    where
    server :: ServerT UAPI AppM
    server = serveDirectoryFileServer "./web/dist"
      :<|> createUser
      :<|> login
      :<|> rootHandler

createUser :: CreateUserRequest -> AppM EmptyOk
createUser req = do
  (State pool _) <- ask
  token <- liftIO DB.genApiToken
  pwHash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy . encodeUtf8 $ req ^. password
  case pwHash of
    Nothing -> fail "error processing password"
    hash -> do
      _ <-
        liftIO $
        withResource pool $ \conn ->
          runBeamPostgres conn $ do
            runInsert $
              insert (DB._repoUsers DB.repoDb) $
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
      return EmptyOk

login :: LoginRequest -> AppM ((Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent))
login r = do
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
               (\u -> (u ^. DB.userEmail) ==. val_ reqEmail)
               (all_ (DB._repoUsers DB.repoDb)))
        case matchingUser of
          Nothing -> do
            liftIO . putStrLn $ printf "%s not found" reqEmail
            return $ Left "Incorrect details"
          (Just user) ->
            if validatePassword
                 (encodeUtf8 $ user ^. DB.userPassword)
                 (encodeUtf8 $ r ^. password)
              then return $ Right user
              else return $ Left "Your credentials are invalid."
  case result of
    Left err -> do
      liftIO $ putStrLn err
      throwError $ err401 {errBody="incorrect details"}
    Right user -> do
      applyCookieResult <- liftIO $ acceptLogin defaultCookieSettings (defaultJWTSettings jwk) (Session (reqEmail) (DB._userPassword user))
      case applyCookieResult of
        Nothing         -> fail "coun't apply cookie"
        (Just applyRes) -> return $ applyRes NoContent

rootHandler :: [Text] -> AppM Html
rootHandler path =
  if null path
    then liftIO $ BZ.preEscapedToHtml <$> readFile "./web/dist/index.html"
    else throwError $ err404 {errBody = "Not found"}

main =
  let port = 9001
  in do connPool <- DB.initConnectionPool connString
        now <- getCurrentTime
        jwkKey <- generateKey
        putStrLn $ "serving on " <> show port
        run 9001 $ app $ State connPool jwkKey
