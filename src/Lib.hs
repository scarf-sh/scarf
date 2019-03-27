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
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Lib where

import           Common
import qualified Models                     as DB
import           PackageSpec

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Crypto.JOSE.JWK
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.List
import           Data.Pool
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Text.IO               hiding (putStrLn)
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock.POSIX
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID4
import           Database.Beam.Postgres
import qualified Dhall                      as Dhall
import           DynFlags
import           GHC.Generics
import           Lens.Micro.Platform
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Prelude                    hiding (FilePath, writeFile)
import           Servant.Auth.Server
import           System.Directory
import           System.Exit
import           System.IO                  (hClose, hPutStr)
import           System.Posix.Files
import           System.Posix.Types
import           System.Process.Typed
import           Text.Pretty.Simple
import           Text.Printf


type FilePath = Text

delimeter :: Text
delimeter = "----"

toString = T.unpack
toText = T.pack

data Config = Config
  { homeDirectory :: FilePath
  , apiToken      :: Text
  }

data ExecutionResult = ExecutionResult
  { result    :: ExitCode
  , runtimeMS :: Integer
  , args      :: [Text]
  } deriving (Show)

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

authCheck :: Pool Connection
          -> BasicAuthData
          -> IO (AuthResult Session)
authCheck connPool (BasicAuthData _ apiToken) = do
  userLookup <- DB.getUserForApiToken connPool (decodeUtf8 apiToken)
  return $ maybe
    Indefinite
    (\user ->
       Authenticated $
       Session (user ^. DB.id) (user ^. DB.email) (user ^. DB.email))
    userLookup

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult Session)

instance FromBasicAuthData Session where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

data CreatePackageCallRequest = CreatePackageCallRequest
  { createPackageCallRequestPackageReleaseUuid :: Text
  , createPackageCallRequestExit               :: Integer
  , createPackageCallRequestRunTimeMs          :: Integer
  , createPackageCallRequestArgString          :: Text
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CreatePackageCallRequest"}
  ''CreatePackageCallRequest
makeFields ''CreatePackageCallRequest

data  CreatePackageRequest = CreatePackageRequest {
  createPackageRequestName             :: Text,
  createPackageRequestShortDescription :: Text,
  createPackageRequestLongDescription  :: Maybe Text
                                                  }
deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CreatePackageRequest"}
  ''CreatePackageRequest
makeFields ''CreatePackageRequest

data GetPackagesResponse = GetPackagesResponse {
  getPackagesResponsePackages :: [DB.Package]
                                               }
deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "GetPackagesResponse"}
  ''GetPackagesResponse
makeFields ''GetPackagesResponse

data PackageDetailsResponse = PackageDetailsResponse
  { packageDetailsResponsePackage  :: DB.Package
  , packageDetailsResponseReleases :: [DB.PackageRelease]
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageDetailsResponse"}
  ''PackageDetailsResponse
makeFields ''PackageDetailsResponse

data CreatePackageReleaseRequest = CreatePackageReleaseRequest
  { createPackageReleaseRequestRawDhall :: Text
  } deriving (Show)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CreatePackageReleaseRequest"}
  ''CreatePackageReleaseRequest
makeFields ''CreatePackageReleaseRequest

exitNum :: ExitCode -> Integer
exitNum ExitSuccess     = 0
exitNum (ExitFailure i) = fromIntegral i

runProgramWrapped :: (MonadReader Config m, MonadIO m) => FilePath -> Text -> m ExecutionResult
runProgramWrapped f argString =
  let argsToPass = filter (/= "") (T.splitOn delimeter argString)
      uuid = head $ T.splitOn delimeter f
  in do home <- asks homeDirectory
        start <- liftIO $ (round . (* 1000)) `fmap` getPOSIXTime
        exitCode <-
          runProcess $
          proc (toString $ originalProgram home f) (map toString argsToPass)
        end <- liftIO $ (round . (* 1000)) `fmap` getPOSIXTime
        let runtime = (end - start)
            packageCallToLog =
              CreatePackageCallRequest uuid (exitNum exitCode) runtime argString
        -- TODO(#configuration) - the server should be configurable
        initReq <- liftIO $ parseRequest "http://localhost:9001/package-call"
        let request =
              setRequestBodyJSON packageCallToLog $ initReq {method = "POST"}
        -- TODO - logging
        -- liftIO $ print request
        response <- httpBS request
        -- liftIO $ print response
        return $ ExecutionResult exitCode runtime argsToPass

uploadPackageRelease :: (MonadReader Config m, MonadIO m) => FilePath -> m ()
uploadPackageRelease f = do
  home <- asks homeDirectory
  token <- asks apiToken
  let adjustedF = T.replace "~" home f
  dhallRaw <- liftIO . TIO.readFile $ T.unpack adjustedF
  (parsedPackageSpec :: Either DhallError PackageSpec.PackageSpec) <-
    liftIO $ parseDhallEither adjustedF
  case parsedPackageSpec of
    (Left err) -> liftIO . putStrLn $ show err
    (Right spec) -> do
      liftIO $ putStrLn "Uploading release"
      pPrint spec
      let requestBody = CreatePackageReleaseRequest dhallRaw
      initReq <- liftIO $ parseRequest "http://localhost:9001/package/release"
      let request =
            (setRequestBasicAuth "none" (encodeUtf8 token)) .
            (setRequestBodyJSON requestBody) $
            initReq {method = "POST"}
      response <- httpBS request
      if (getResponseStatusCode response) == 200
        then pPrint "Upload complete!"
        else pPrint $ getResponseBody response


originalProgram homeFolder fileName = homeFolder <>  "/.u/original/" <> fileName

wrappedProgram homeFolder fileName = homeFolder <> "/.u/bin/" <> fileName

type ExecutableId = Text

type Username = Text
type PackageName = Text

installProgramWrapped :: (MonadReader Config m, MonadIO m) => FilePath -> ExecutableId -> m ()
installProgramWrapped f uuid =
  let fileName = last $ T.splitOn "/" f
  in do home <- asks homeDirectory
        let wrappedProgramPath = (toString $ wrappedProgram home fileName)
        liftIO $
          copyFile
            (toString (T.replace "~" home f))
            (toString $ originalProgram home $ uuid <> delimeter <> fileName)
    -- TODO(#bug) conflicting filenames breaks stuff
        liftIO $
          writeFile
            wrappedProgramPath
            (T.unlines
               [ "#!/bin/bash"
               , "function join_by { local d=$1; shift; echo -n \"$1\"; shift; printf \"%s\" \"${@/#/$d}\"; }"
               , toText $ printf "arg_string=$(join_by \"%s\" \"$@\")" delimeter
               , toText $
                 printf
                   "u-exe execute %s%s%s --args \"$arg_string\""
                   uuid
                   delimeter
                   fileName
               ])
        liftIO $ setFileMode wrappedProgramPath accessModes
        liftIO $ printf "Installation complete: %s\n" wrappedProgramPath

lintDhallPackageFile :: (MonadReader Config m, MonadIO m) => FilePath -> m (Either DhallError PackageSpec.PackageSpec)
lintDhallPackageFile f = do
  home <- asks homeDirectory
  let pathFixed = (T.replace "~" home f)
  (parsedPackage :: Either DhallError PackageSpec) <-
    liftIO $ parseDhallEither pathFixed
  liftIO . putStrLn . T.unpack $
    either
      (\err -> "Couldn't parse package spec: " <> unDhallError err)
      (\parsed -> T.pack $ show parsed)
      parsedPackage
  return parsedPackage

newtype DhallError = DhallError { unDhallError :: Text } deriving (Show)

parseDhallEither :: (MonadIO m, Dhall.Interpret a) => FilePath -> m (Either DhallError a)
parseDhallEither f =
  liftIO $ catch
    (Right <$> Dhall.input Dhall.auto f)
    (\(err :: SomeException) -> return . Left . DhallError . T.pack $ show err)

textUUID :: MonadIO m => m Text
textUUID = liftIO $ UUID.toText <$> UUID4.nextRandom


intersection :: Eq a => [a] -> [a] -> [a]
intersection (x:xs) ys = if x `elem` ys
                 then x:intersection xs (delete x ys)
                 else intersection xs ys
intersection [] _ = []

