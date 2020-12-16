{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DerivingVia            #-}
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

module Scarf.Types where

import           Scarf.Common
import           Scarf.PackageSpec

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Text                 (Text)
import           Data.Time.Clock
import           Distribution.License
import           Distribution.Parsec.Class
import           Distribution.Pretty
import           Distribution.Version
import           GHC.Generics
import           Lens.Micro.Platform hiding ((.=))
import           Prelude                   hiding (FilePath, writeFile)
import           System.Exit

data UserTier
  = FreeTier -- ^ Base tier, free to use
  | PrivateTier -- ^ Customer purchased package to use without data collection
  deriving (Show, Eq, Read, Generic)

instance ToJSON UserTier
instance FromJSON UserTier

data ExecutionResult = ExecutionResult
  { result    :: ExitCode
  , runtimeMS :: Integer
  , args      :: [Text]
  } deriving (Show)

data CreateUserRequest = CreateUserRequest
  { createUserRequestEmail       :: Text
  , createUserRequestUsername    :: Text
  , createUserRequestPassword    :: Maybe Text
  , createUserRequestGitHubState :: Maybe Text
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CreateUserRequest"}
  ''CreateUserRequest
makeFields ''CreateUserRequest

data CreateUserResponse =
  CreateUserResponse
    { createUserResponseApiToken :: Text
    }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CreateUserResponse"}
  ''CreateUserResponse
makeFields ''CreateUserResponse

data LoginRequest = LoginRequest
  { loginRequestEmail       :: Text
  , loginRequestPassword    :: Text
  , loginRequestGitHubState :: Maybe Text
  }

deriveJSON
  defaultOptions {fieldLabelModifier = makeFieldLabelModfier "loginRequest"}
  ''LoginRequest
makeFields ''LoginRequest

data CreatePackageCallRequest = CreatePackageCallRequest
  { createPackageCallRequestReleaseUuid :: Text
  , createPackageCallRequestExit        :: Integer
  , createPackageCallRequestRunTimeMs   :: Integer
  , createPackageCallRequestArgString   :: Text
  , createPackageCallRequestBinaryAlias :: Maybe Text
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CreatePackageCallRequest"}
  ''CreatePackageCallRequest
makeFields ''CreatePackageCallRequest

data CreatePackageRequest =
  CreatePackageRequest
    { createPackageRequestName :: Text
    , createPackageRequestShortDescription :: Text
    , createPackageRequestLongDescription :: Maybe Text
    , createPackageRequestWebsite :: Maybe Text
    , createPackageRequestExternalLibraryType :: Maybe Scarf.PackageSpec.ExternalLibraryType
    , createPackageRequestGatewayPublicUrl :: Maybe Text
    , createPackageRequestGatewayBackendUrl :: Maybe Text
    }
deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CreatePackageRequest"}
  ''CreatePackageRequest
makeFields ''CreatePackageRequest

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
  { packageStatPackage       :: Text
  , packageStatVersion       :: Version
  , packageStatPlatform      :: Scarf.PackageSpec.Platform
  , packageStatExit          :: Integer
  , packageStatCount         :: Integer
  , packageStatAverageTimeMs :: Integer
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

data PackageSummary = PackageSummary {
  packageSummaryUuid           :: Text,
  packageSummaryName           :: Text,
  packageSummaryOwner          :: Text,
  packageSummaryLicense        :: License,
  packageSummaryLatestReleases :: [(Scarf.PackageSpec.Platform, Version)]
}

data Package =
  Package
    { packageUuid                :: Text
    , packageOwner               :: Text
    , packageName                :: Text
    , packageShortDescription    :: Text
    , packageLongDescription     :: Maybe Text
    , packageCreatedAt           :: UTCTime
    , packageWebsite             :: Maybe Text
    , packageExternalLibraryType :: Maybe Scarf.PackageSpec.ExternalLibraryType
    }
  deriving (Show)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "Package"}
  ''Package
makeFields ''Package

data InstallPlan =
  InstallPlan
    { installPlanExternalPackageType :: Maybe Scarf.PackageSpec.ExternalPackageType
    , installPlanApplications        :: Scarf.PackageSpec.ReleaseApplicationObject
    , installPlanInstallCommand      :: Maybe Text
    }
  deriving (Show, Eq)
deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "InstallPlan"}
  ''InstallPlan
makeFields ''InstallPlan

data PackageType
  = ArchivePackage
  | NodePackage
  | ExternalPackage
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PackageRelease = PackageRelease {
    packageReleaseUuid                    :: Text
  , packageReleaseName                    :: Text
  , packageReleaseUploaderName            :: (Maybe Text)
  , packageReleaseAuthor                  :: Text
  , packageReleaseCopyright               :: Text
  , packageReleaseLicense                 :: License
  , packageReleaseVersion                 :: Version
  , packageReleasePlatform                :: Scarf.PackageSpec.Platform
  , packageReleaseExecutableUrl           :: Maybe Text
  , packageReleaseExecutableSignature     :: Maybe Text
  , packageReleaseSimpleExecutableInstall :: Maybe Text
  , packageReleasePackageType             :: PackageType
  , packageReleaseNodePackageJson         :: Maybe Text
  , packageReleaseIncludes                :: [Text]
  , packageReleaseDepends                 :: Dependencies
  , packageReleaseCreatedAt               :: UTCTime
  , packageReleaseInstallPlans            :: [InstallPlan]
  , packageReleaseScripts                 :: [Scarf.PackageSpec.PackageScript]
                                     } deriving (Show, Eq)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageRelease"}
  ''PackageRelease
makeFields ''PackageRelease

data PackageReleaseWithGraphContext = PackageReleaseWithGraphContext PackageRelease [PackageRelease] deriving (Eq, Show)

newtype DockerNamespace = DockerNamespace { unDockerNamespace :: Text }
  deriving (Eq, Generic, Ord, Show)

deriveJSON
  defaultOptions { unwrapUnaryRecords = True}
  ''DockerNamespace

newtype DockerImage = DockerImage { unDockerImage :: Text }
  deriving (Eq, Generic, Ord, Show)
  deriving ToJSON via DockerNamespace
  deriving FromJSON via DockerNamespace

data GatewayPath
  = DockerPath (Maybe DockerNamespace) DockerImage
  | OtherPath Text
  deriving (Eq, Generic, Ord, Show)

instance ToJSON GatewayPath where
  toJSON (DockerPath dockerNamespace (DockerImage dockerImage)) =
    object [ "tag"       .= ("DockerPath" :: Text)
           , "namespace" .= dockerNamespace
           , "image"     .= dockerImage
           ]
  toJSON (OtherPath t) = object [ "tag"  .= ("OtherPath" :: Text)
                                , "path" .= t ]

instance FromJSON GatewayPath where
  parseJSON = withObject "GatewayPath" $ \gp -> do
    tag :: Text <- gp .: "tag"
    case tag of
      "DockerPath" -> do
        namespace <- gp .:? "namespace"
        image     <- gp .: "image"
        return $ DockerPath (DockerNamespace <$> namespace) (DockerImage image)
      _ -> do
        path <- gp .:? "path"
        return $ OtherPath $ fromMaybe "" path

data GatewayBackendUrl = GatewayBackendUrl
  { gatewayBackendUrlDomain :: Text,
    gatewayBackendUrlPath :: GatewayPath
  }
  deriving (Eq, Generic, Ord, Show)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "GatewayBackendUrl"}
  ''GatewayBackendUrl

newtype GatewayPublicUrl = GatewayPublicUrl { unGatewayPublicUrl :: Text }
  deriving (Eq, Generic, Ord, Show)
  deriving ToJSON via DockerNamespace
  deriving FromJSON via DockerNamespace

data GatewayConfig = GatewayConfig { gatewayConfigBackendUrl           :: Maybe GatewayBackendUrl
                                   , gatewayConfigPublicUrl            :: Maybe GatewayPublicUrl
                                   , gatewayConfigDefaultBackendDomain :: Maybe Text
                                   , gatewayConfigSanitizedUsername    :: Text
                                   } deriving (Eq, Generic, Ord, Show)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "GatewayConfig"}
  ''GatewayConfig

data PackageDetails = PackageDetails
  { packageDetailsPackage       :: Package
  , packageDetailsOwnerName     :: Text
  , packageDetailsReleases      :: [PackageRelease]
  , packageDetailsLicense       :: Maybe License
  , packageDetailsTotalInstalls :: Integer
  , packageDetailsGatewayConfig :: Maybe GatewayConfig
  } deriving (Show)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageDetails"}
  ''PackageDetails
makeFields ''PackageDetails

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageSummary"}
  ''PackageSummary
makeFields ''PackageSummary

data PackageSearchResults = PackageSearchResults {
  packageSearchResultsQuery   :: Text,
  packageSearchResultsResults :: [PackageDetails]
}

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageSearchResults"}
  ''PackageSearchResults
makeFields ''PackageSearchResults

data ValidatedPackageSpec = ValidatedPackageSpec {
  validatedPackageSpecName          :: Text,
  validatedPackageSpecVersion       :: Version,
  validatedPackageSpecAuthor        :: Text,
  validatedPackageSpecCopyright     :: Text,
  validatedPackageSpecLicense       :: License,
  validatedPackageSpecDistributions :: [Scarf.PackageSpec.PackageDistribution]
} deriving (Show, Generic)

instance ToJSON Version where
  toJSON ver = String (toText $ prettyShow ver)

instance FromJSON Version where
  parseJSON (String versionString) =
    either
      (const . fail $ "unparsable version string: " ++ (toString versionString))
      (return)
      (eitherParsec $ toString versionString)
  parseJSON a = typeMismatch "needed a version string" a

instance ToJSON License
instance FromJSON License

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "ValidatedPackageSpec"}
  ''ValidatedPackageSpec
makeFields ''ValidatedPackageSpec

data PackageCall = PackageCall {
  packageCallExit        :: Integer,
  packageCallTimeMs      :: Integer,
  packageCallArgsList    :: [Text],
  packageCallBinaryAlias :: Maybe Text,
  packageCallCreatedAt   :: UTCTime
}

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageCall"}
  ''PackageCall
makeFields ''PackageCall

data PackageCallsResponse = PackageCallsResponse {
  packageCallsResponseCalls :: [PackageCall]
                                                 }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageCallsResponse"}
  ''PackageCallsResponse
makeFields ''PackageCallsResponse

data UserInstallation = UserInstallation
  { userInstallationName                :: Text
  , userInstallationAlias               :: Maybe Text
  , userInstallationUuid                :: Maybe Text
  , userInstallationVersion             :: Maybe Text
  , userInstallationPackageType         :: Maybe PackageType
  , userInstallationExternalPackageType :: Maybe Scarf.PackageSpec.ExternalPackageType
  , userInstallationTarget              :: Maybe Text
  , userInstallationRunner              :: Maybe Text
  } deriving (Show, Eq)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "UserInstallation"}
  ''UserInstallation
makeFields ''UserInstallation

type PackageAccessEntry = (Text, UserTier)

data UserState = UserState
  { userStateDepends       :: Maybe [UserInstallation]
  , userStatePackageAccess :: Maybe [PackageAccessEntry]
  } deriving (Show)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "UserState"}
  ''UserState
makeFields ''UserState

getDependencies :: UserState -> [UserInstallation]
getDependencies (UserState Nothing _)  = []
getDependencies (UserState (Just l) _) = l

data CliVersionResponse = CliVersionResponse
  { cliVersionResponseVersion :: Text
  } deriving (Show)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CliVersionResponse"}
  ''CliVersionResponse
makeFields ''CliVersionResponse

data LatestPackageIndex = LatestPackageIndex
  { latestPackageIndexIndex :: [PackageRelease]
  } deriving (Show)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "LatestPackageIndex"}
  ''LatestPackageIndex
makeFields ''LatestPackageIndex

data LatestPackageIndexRequest = LatestPackageIndexRequest
  { latestPackageIndexRequestPlatform :: Platform
  } deriving (Show)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "LatestPackageIndexRequest"}
  ''LatestPackageIndexRequest
makeFields ''LatestPackageIndexRequest

data SyncPackageAccessResponse = SyncPackageAccessResponse {
  syncPackageAccessResponseAccessList :: [PackageAccessEntry]
                                                           } deriving (Show)
deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "SyncPackageAccessResponse"}
  ''SyncPackageAccessResponse
makeFields ''SyncPackageAccessResponse

data FeedbackRequest =
  FeedbackRequest
    { feedbackRequestEmail :: Text
    , feedbackRequestName  :: Text
    , feedbackRequestBody  :: Text
    }

deriveJSON
  defaultOptions {fieldLabelModifier = makeFieldLabelModfier "FeedbackRequest"}
  ''FeedbackRequest
makeFields ''FeedbackRequest

data PackagePermissionLevel
  = Blocked
  | DefaultPermission
  | Member
  | Admin
  | Owner
  deriving (Show, Eq, Ord, Enum, Read)

deriveJSON
  defaultOptions {fieldLabelModifier = makeFieldLabelModfier "PackagePermissionLevel"}
  ''PackagePermissionLevel
makeFields ''PackagePermissionLevel

