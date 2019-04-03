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

module Types where

import           Common
import qualified Models                 as DB
import           PackageSpec

import           Data.Aeson.TH
import           Data.Maybe
import           Data.Pool
import           Data.Text              (Text)
import           Data.Text.Encoding
import           Database.Beam
import           Database.Beam.Postgres
import           Lens.Micro.Platform
import           Network.HTTP.Client    (Manager, defaultManagerSettings,
                                         newManager)
import           Prelude                hiding (FilePath, writeFile)
import           Servant.Auth.Server
import           System.Exit

data Config = Config
  { homeDirectory :: FilePath
  , userApiToken  :: Maybe Text
  , httpManager   :: Manager
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

data PackageDetails = PackageDetails
  { packageDetailsPackage  :: DB.Package
  , packageDetailsReleases :: [DB.PackageRelease]
  } deriving (Show)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageDetails"}
  ''PackageDetails
makeFields ''PackageDetails

data GetUserAccountDetailsResponse = GetUserAccountDetailsResponse
  { getUserAccountDetailsResponseApiToken :: Text
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "GetUserAccountDetailsResponse"}
  ''GetUserAccountDetailsResponse
makeFields ''GetUserAccountDetailsResponse

data UpdatePasswordRequest = UpdatePasswordRequest
  { updatePasswordRequestCurrentPassword :: Text
  , updatePasswordRequestNewPassword     :: Text
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "UpdatePasswordRequest"}
  ''UpdatePasswordRequest
makeFields ''UpdatePasswordRequest

data PackageStat = PackageStat
  { packageStatPackageName   :: Text
  , packageStatVersion       :: Text
  , packageStatPlatform      :: PackageSpec.Platform
  , packageStatExit          :: Integer
  , packageStatCount         :: Integer
  , packageStatAverageTimeMs :: Double
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageStat"}
  ''PackageStat
makeFields ''PackageStat


data PackageStatsResponse = PackageStatsResponse
  { packageStatsResponseStats :: [PackageStat]
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageStatsResponse"}
  ''PackageStatsResponse
makeFields ''PackageStatsResponse

