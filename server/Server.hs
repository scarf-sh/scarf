{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}


module Main where

import           Api
import           Common
import           Lib
import qualified Models                                   as DB
import qualified PackageSpec                              as PackageSpec
import           Types

import qualified Aws
import qualified Aws.S3                                   as S3
import qualified Aws.S3.Core                              as S3C
import           Conduit
import           Control.Monad.Reader                     (MonadReader, ReaderT,
                                                           ask, asks,
                                                           runReaderT)
import           Crypto.BCrypt
import           Crypto.JOSE.JWK
import           Data.Aeson                               (FromJSON, ToJSON)
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.ByteArray.Encoding
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Char8                    as B8S
import qualified Data.ByteString.Lazy                     as BSL
import qualified Data.ByteString.Lazy.Char8               as B8SL
import           Data.Foldable
import qualified Data.List
import           Data.Maybe
import           Data.Pool
import qualified Data.SemVer
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           Data.Text.Encoding
import           Data.Time.Clock
import           Database.Beam
import qualified Database.Beam.Backend.SQL.BeamExtensions as Extensions
import           Database.Beam.Postgres
import           Lens.Micro.Platform
import qualified Network.HTTP.Client                      as HTTP
import qualified Network.HTTP.Client.TLS                  as TLS
import           Network.Wai.Handler.Warp                 (run)
import           Prelude                                  hiding (FilePath)
import           Servant
import           Servant.Auth.Server
import           Servant.Client
import qualified Servant.Client.Streaming                 as S
import           Servant.HTML.Blaze                       (HTML)
import           Servant.Multipart
import           System.Environment
import           Text.Blaze.Html5                         (Html)
import qualified Text.Blaze.Html5                         as BZ

import           Text.Pretty.Simple
import           Text.Printf


connString = "postgres://avipress@127.0.0.1:5432/udb?sslmode=disable"

data State = State
  { connPool    :: Pool Connection
  , awsConfig   :: Aws.Configuration
  , s3Config    :: S3.S3Configuration Aws.NormalQuery
  , httpManager :: HTTP.Manager
  }
type AppM = ReaderT State Handler

toLazyBS = BSL.fromStrict . encodeUtf8

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: Servant.Context '[CookieSettings, JWTSettings, BasicAuthCfg] -> CookieSettings -> JWTSettings -> BasicAuthCfg -> State -> Application
app cfg cookieSettings jwtSettings basicAuthSettings state =
  serveWithContext fullAPI cfg $
  hoistServerWithContext
    fullAPI
    (Proxy :: Proxy '[ CookieSettings, JWTSettings, BasicAuthCfg])
    (nt state)
    (fullServer cookieSettings jwtSettings)

static :: ServerT StaticAPI AppM
static =
  serveDirectoryFileServer "./web/dist"
  :<|> rootHandler

unprotected :: CookieSettings -> JWTSettings -> ServerT OpenAPI AppM
unprotected cs jwts =
  (createUser cs jwts)
  :<|> (login cs jwts)
  :<|> getPackageDetailsHandler
  :<|> searchPackagesHander

protected :: AuthResult Session -> ServerT ProtectedAPI AppM
protected (Authenticated s) =
  isLoggedInHandler s :<|> createPackageHandler s :<|>
  uploadPackageReleaseArtifact s :<|>
  getPackageStatsHandler s :<|>
  getPackagesHandler s :<|>
  getUserAccountDetailsHander s :<|>
  regenerateApiTokenHandler s :<|>
  updatePasswordHandler s
protected _                 = throwAll err401

optionallyProtected :: AuthResult Session -> ServerT OptionallyProtectedAPI AppM
optionallyProtected (Authenticated s) = optionallyProtectedImpl $ Just s
optionallyProtected  Indefinite       = optionallyProtectedImpl Nothing
optionallyProtected  _                = throwAll err401

optionallyProtectedImpl :: Maybe Session -> ServerT OptionallyProtectedAPI AppM
optionallyProtectedImpl s =
  createPackageCallHandler s

fullServer :: CookieSettings -> JWTSettings -> ServerT (FullAPI auths) AppM
fullServer cs jwts = protected :<|> optionallyProtected :<|> unprotected cs jwts :<|> static

createUser ::
     CookieSettings
  -> JWTSettings
  -> CreateUserRequest
  -> AppM ((Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent))
createUser cSettings jSettings req = do
  pool <- asks connPool
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
            inserted <- Extensions.runInsertReturningList $ insert (DB._repoUsers DB.repoDb) $
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

getPackagesHandler :: Session -> AppM GetPackagesResponse
getPackagesHandler session = do
  pool <- asks connPool
  packages <-
    liftIO $
    withResource pool $ \conn ->
      runBeamPostgresDebug putStrLn conn $ do
        runSelectReturningList $
          select
            (filter_
                (\p ->
                  ((p ^. DB.owner) ==. (val_ . DB.UserId $ session ^. userId)))
                (all_ (DB._repoPackages DB.repoDb)))
  return $ GetPackagesResponse packages

allReleasesForPackagesWithQueryQ q = do
  pkgs <- (filter_
      q
      (all_ (DB._repoPackages DB.repoDb)))
  pkgRelease <-
    oneToMany_
      (DB._repoPackageReleases DB.repoDb)
      DB.packagereleasePackage
      pkgs
  pure (pkgs, releases)

searchPackagesHander :: Text -> AppM PackageSearchResults
searchPackagesHander q = do
  pool <- asks connPool
  results <- liftIO $
    withResource pool $ \conn -> do
      matchingPackages <- runBeamPostgresDebug putStrLn conn $ do
        runSelectReturningList $
          select $ do
            (filter_
                (\p -> ((p ^. DB.name) `like_` (val_ $ "%" <> q <> "%")))
              (all_ (DB._repoPackages DB.repoDb)))
      -- _ <- liftIO . print $ matchingPackages
      details <- mapM (\result -> getPackageDetails conn (result ^. DB.name)) matchingPackages
      -- TODO search functionality needs some paging
      return . take 25 $ getJusts details
  return $ PackageSearchResults q results

createPackageCallHandler :: Maybe Session -> CreatePackageCallRequest -> AppM NoContent
createPackageCallHandler maybeSession req = do
  let callerUserId = (^. userId) <$> maybeSession
  pool <- asks connPool
  _ <-
    liftIO $
    withResource pool $ \conn ->
      runBeamPostgresDebug putStrLn conn $ do
        insertResult <-
          runInsert $
          insert (DB._repoPackageCalls DB.repoDb) $
          insertExpressions
            [ DB.PackageCall
                default_
                (val_ $ DB.PackageReleaseId $ req ^. packageReleaseUuid)
                (val_ $ DB.UserId callerUserId)
                (val_ $ req ^. exit)
                (val_ $ req ^. runTimeMs)
                (val_ $ req ^. argString)
                (default_)
            ]
        return ()
  return NoContent

createPackageHandler :: Session -> CreatePackageRequest -> AppM NoContent
createPackageHandler session request = do
  pool <- asks connPool
  uuid <- textUUID
  maybeError <-
    liftIO $
    withResource pool $ \conn ->
      runBeamPostgresDebug putStrLn conn $ do
        existingPackage <-
          runSelectReturningList $
          select
            (filter_
               (\p ->
                  ((p ^. DB.name) ==. (val_ $ request ^. name)) &&.
                  ((p ^. DB.owner) ==. (val_ . DB.UserId $ session ^. userId)))
               (all_ (DB._repoPackages DB.repoDb)))
        case existingPackage of
          [] -> do
            runInsert $
              insert (DB._repoPackages DB.repoDb) $
              insertExpressions
                [ DB.Package
                    default_
                    (val_ uuid)
                    (val_ . DB.UserId $ session ^. userId)
                    (val_ $ request ^. name)
                    (val_ $ request ^. shortDescription)
                    (val_ $ request ^. longDescription)
                    (default_)
                ]
            return (Nothing :: Maybe Text)
          p ->
            return . Just . T.pack $
            printf "'%s' is not an available package name" $ request ^. name
  case maybeError of
    (Just message) -> throwAll err400 {errBody = BSL.fromStrict $ encodeUtf8 message}
    Nothing        -> return NoContent

createPackageReleaseHandler :: Session -> PackageSpec.PackageSpec -> AppM NoContent
createPackageReleaseHandler session spec = do
  pool <- asks connPool
  result <-
    liftIO $
    withResource pool $ \conn ->
      runBeamPostgresDebug putStrLn conn $
        -- lookup package
        do
        maybeFetchedPackage <-
          runSelectReturningOne $
          select $
          (filter_
              (\p ->
                ((p ^. DB.name) ==. (val_ $ PackageSpec.name spec)) &&.
                ((p ^. DB.owner) ==. (val_ . DB.UserId $ session ^. userId)))
              (all_ (DB._repoPackages DB.repoDb)))
        case maybeFetchedPackage of
          Nothing -> return $ Left (404, "Package not found")
          (Just fetchedPackage) -> do
            releases <-
              runSelectReturningList $
              select $
              (filter_
                  (\r ->
                    (r ^. DB.package ==.
                      (val_ . DB.PackageId $ fetchedPackage ^. DB.uuid)))
                  (all_ (DB._repoPackageReleases DB.repoDb)))
            -- insert releases for package
            let fetchedPlatformAndVersions =
                  map (\r -> (r ^. DB.platform, r ^. DB.version)) releases
                requestPlatformAndVersions =
                  map
                    (\r ->
                        (PackageSpec.platform r, PackageSpec.version spec))
                    (PackageSpec.distributions spec)
                duplicates =
                  intersection
                    fetchedPlatformAndVersions
                    requestPlatformAndVersions
            if (not $ null duplicates)
              then return $
                    Left
                      ( 400
                      , "Package releases already exists: " <>
                        (toText $ show duplicates))
              else do
                traverse_
                  (\(distribution :: PackageSpec.PackageDistribution) -> do
                      uuid <- liftIO textUUID
                      let uri = if isRemoteUrl $ PackageSpec.uri distribution
                            then PackageSpec.uri distribution
                            else hostedArchiveUrl spec distribution
                      runInsert $
                        insert (DB._repoPackageReleases DB.repoDb) $
                        insertExpressions
                          [ DB.PackageRelease
                              (default_)
                              (val_ uuid)
                              (val_ . DB.PackageId $
                              fetchedPackage ^. DB.uuid)
                              (val_ . DB.UserId $ session ^. userId)
                              (val_ $ PackageSpec.version spec)
                              (val_ $ PackageSpec.platform distribution)
                              (val_ $ uri)
                              (val_ $ PackageSpec.signature distribution)
                              (val_ $ PackageSpec.simpleExecutableInstall distribution)
                              (default_)
                          ])
                  (PackageSpec.distributions spec)
                return $ Right ()
  either
    (\(code, msg) ->
        case code of
          400 -> throwAll $ err400 {errBody = toLazyBS msg}
          404 -> throwAll $ err404 {errBody = toLazyBS msg})
    (const $ return NoContent)
    result

hostedArchiveUrl :: PackageSpec.PackageSpec -> PackageSpec.PackageDistribution -> Text
hostedArchiveUrl spec dist =
  "https://s3-us-west-2.amazonaws.com/" <> "u-test-repo/" <>
  mkObjectName spec (toText . show $ PackageSpec.platform dist)

mkObjectName spec platform =
  (PackageSpec.name spec <> "-" <> PackageSpec.version spec <> "-" <>
   (T.toLower platform) <>
   ".tar.gz")

uploadPackageReleaseArtifact :: Session -> MultipartData Mem -> AppM NoContent
uploadPackageReleaseArtifact session multipartData = do
  specFile <-
    maybe (throwAll $ err400 {errBody = "No spec found"}) return $
    lookupFile "spec" multipartData
  liftIO . putStrLn . toString $ fdFileName specFile
  (parsedSpec :: Either DhallError PackageSpec.PackageSpec) <-
    parseDhallEither (decodeUtf8 . B8SL.toStrict $ fdPayload specFile)
  case parsedSpec of
    Left err ->
      throwAll $
      err400
      { errBody =
          toLazyBS $
          "Invalid package spec. Try using 'check-package'. Error: " <>
          (toText $ show err)
      }
    Right spec -> do
      _ <- createPackageReleaseHandler session spec
      aws <- asks awsConfig
      s3 <- asks s3Config
      http <- asks Main.httpManager
      let archives =
            filter (\f -> "archive-" `T.isPrefixOf` fdInputName f) $
            files multipartData
      mapM_
        (\archive -> do
           let archiveBytes = fdPayload archive
                -- TODO sanitize package input
           let objectName =
                 mkObjectName
                   spec
                   (platformNameForArchive $ fdInputName archive)
           let objectBytes = (B8SL.toStrict archiveBytes)
           liftIO $
             runResourceT $ do
               S3.PutObjectResponse {S3.porVersionId = rsp} <-
                 Aws.pureAws aws s3 http $
                 (S3.putObject
                    "u-test-repo"
                    objectName
                    (HTTP.RequestBodyBS objectBytes))
                 {S3.poAcl = Just S3C.AclPublicRead}
               return ())
        archives
      return NoContent

-- archive-Linux_x86_64 -> Linux_x86_64
platformNameForArchive :: Text -> Text
platformNameForArchive = T.replace "archive-" ""

getPackageDetailsHandler :: PackageName -> AppM PackageDetails
getPackageDetailsHandler packageName = do
  pool <- asks connPool
  maybeResult <- withResource pool (flip getPackageDetails packageName)
  case maybeResult of
    Nothing ->
      throwAll
        err404
        { errBody =
            (BSL.fromStrict . encodeUtf8 $
             "Couldn't find package " <> packageName)
        }
    Just details -> return details

isLoggedInHandler :: Session -> AppM Session
isLoggedInHandler = return

login ::
     CookieSettings
  -> JWTSettings
  -> LoginRequest
  -> AppM ((Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent))
login c j r = do
  pool <- asks connPool
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
        Nothing         -> fail "coulld't apply cookie"
        (Just applyRes) -> return $ applyRes NoContent

getUserAccountDetailsHander :: Session -> AppM GetUserAccountDetailsResponse
getUserAccountDetailsHander session = do
  pool <- asks connPool
  maybeUser <-
    liftIO $
    withResource pool $ \conn ->
      runBeamPostgresDebug putStrLn conn $ do
        runSelectReturningOne $
          select
            (filter_
               (\u -> (u ^. DB.email) ==. (val_ $ session ^. email))
               (all_ (DB._repoUsers DB.repoDb)))
  maybe
    (throwAll $ err500 {errBody = "Something went very wrong"})
    (\user -> return $ GetUserAccountDetailsResponse $ user ^. DB.apiToken)
    maybeUser

regenerateApiTokenHandler :: Session -> AppM GetUserAccountDetailsResponse
regenerateApiTokenHandler session = do
  pool <- asks connPool
  token <- liftIO DB.genApiToken
  result <-
    liftIO $
    withResource pool $ \conn ->
      runBeamPostgresDebug putStrLn conn $ do
        runUpdate $
          update
            (DB._repoUsers DB.repoDb)
            (\user -> DB._userApiToken user <-. val_ token)
            (\u -> (u ^. DB.email) ==. (val_ $ session ^. email))
  getUserAccountDetailsHander session

updatePasswordHandler :: Session -> UpdatePasswordRequest -> AppM NoContent
updatePasswordHandler session request = do
  pool <- asks connPool
  pwHash <-
    liftIO $
    hashPasswordUsingPolicy slowerBcryptHashingPolicy . encodeUtf8 $
    request ^. newPassword
  hash <- maybe (throwM $ err500 {errBody="hash error"}) return pwHash
  result <- liftIO $
    withResource pool $ \conn -> do
      _user <- getUserByEmail conn (session ^. email)
      case _user of
        Nothing -> return $ Left "User not found"
        Just user ->
          if validatePassword
                (encodeUtf8 $ user ^. DB.password)
                (encodeUtf8 $ request ^. currentPassword)
            then do
              runBeamPostgresDebug putStrLn conn $ do
                runUpdate $
                  update
                    (DB._repoUsers DB.repoDb)
                    (\user -> DB._userPassword user <-. (val_ $ decodeUtf8 hash))
                    (\u -> (u ^. DB.email) ==. (val_ $ session ^. email))
              return $ Right ()
            else return $ Left "Current password is incorrect"
  either (\err -> throwM $ err500{errBody=err}) return result
  return NoContent

getPackageStatsHandler :: Session -> PackageName -> AppM PackageStatsResponse
getPackageStatsHandler session pkgName = do
  pool <- asks connPool
  result <-
    liftIO $
    withResource pool $ \conn -> do
      fetchStatsForPackage conn pkgName (session ^. userId)
  return $
    PackageStatsResponse $
    map
      (\(version, platform, exitCode, count, average) ->
         PackageStat
           pkgName
           version
           platform
           exitCode
           (count)
           (average)
      )
      result

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
  awsCreds <- Aws.baseConfiguration
  _httpManager <- HTTP.newManager TLS.tlsManagerSettings
  let s3Config = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
  let cookieSettings =
        defaultCookieSettings
        {cookieIsSecure = NotSecure, cookiePath = (Just "*")}
  let jwtSettings = defaultJWTSettings jwkKey
  let basicAuthSettings = authCheck connPool
  let contextConfig =
        cookieSettings :. jwtSettings :. basicAuthSettings :. EmptyContext
  putStrLn $ "serving on " <> show port
  run 9001 $
    app contextConfig cookieSettings jwtSettings basicAuthSettings $
    State connPool awsCreds s3Config _httpManager
