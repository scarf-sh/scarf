{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Scarf.PackageSpec where

import           Scarf.Common

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.HashMap.Strict        as HM
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding
import qualified Data.Vector                as V
import           Distribution.Pretty
import           Distribution.Version
import           GHC.Generics
import           Prelude                    hiding (FilePath, writeFile)


data Platform = MacOS | Linux_x86_64 | AllPlatforms deriving (Show, Eq, Read, Generic)

instance ToJSON Platform
instance FromJSON Platform

data ExternalPackageType = Homebrew | Debian | RPM | NPM | CPAN
  deriving (Show, Read, Eq, Generic, Enum)
instance ToJSON ExternalPackageType
instance FromJSON ExternalPackageType

externalPackageTypes :: [ExternalPackageType]
externalPackageTypes = [Homebrew ..]

fromPackageManagerBinaryName :: Text -> ExternalPackageType
fromPackageManagerBinaryName "brew"    = Homebrew
fromPackageManagerBinaryName "apt"     = Debian
fromPackageManagerBinaryName "yum"     = RPM
fromPackageManagerBinaryName "npm"     = NPM
fromPackageManagerBinaryName "cpan"    = CPAN
fromPackageManagerBinaryName other     = error . toString $ "could not parse package manager from string: " <> other

toPackageManagerBinaryName :: ExternalPackageType -> Text
toPackageManagerBinaryName Homebrew = "brew"
toPackageManagerBinaryName Debian   = "apt"
toPackageManagerBinaryName RPM      = "yum"
toPackageManagerBinaryName NPM      = "npm"
toPackageManagerBinaryName CPAN     = "cpan"


platformForPackageType :: ExternalPackageType -> Platform
platformForPackageType t = case t of
  Homebrew -> MacOS
  RPM      -> Linux_x86_64
  Debian   -> Linux_x86_64
  NPM      -> AllPlatforms
  CPAN     -> AllPlatforms

data ReleaseApplication =
  ReleaseApplication
    { releaseApplicationName   :: Text
    , releaseApplicationTarget :: Maybe Text
    }
  deriving (Show, Generic, Eq)

newtype ReleaseApplicationObject = ReleaseApplicationObject { unReleaseApplicationObject :: [ReleaseApplication]} deriving (Show, Generic, Eq)

instance FromJSON ReleaseApplicationObject where
  parseJSON (Object o) = return $ fromAesonObject o
  parseJSON (Array a)  = return $ fromAesonArray a
  parseJSON other = typeMismatch "ReleaseApplicationObject should be an object or array of strings" other

fromAesonObject o = ReleaseApplicationObject $
         foldl
           (\acc current ->
              case current of
                (k, String v) -> ReleaseApplication k (Just v) : acc
                (k, Null)     -> ReleaseApplication k Nothing : acc
                (_, _)        -> acc)
           []
           (HM.toList o)

fromAesonArray a = ReleaseApplicationObject $
         foldl
           (\acc current ->
              case current of
                (String k) -> ReleaseApplication k (Nothing) : acc
                _          -> acc)
           []
           (a)

instance ToJSON ReleaseApplicationObject where
  toJSON (ReleaseApplicationObject l) = object (map (\(ReleaseApplication k v) -> k .= v) l)

data PackageDistribution
  = ArchiveDistribution
      { archiveDistributionPlatform                :: Platform
                        -- remote url or local file path to a tar.gz archive of your binary and
                        -- optional data paths https:// or ./
      , archiveDistributionUri                     :: Text
                        -- we'll enable signature once the checking is implemented
                        -- signature               :: Maybe Text,
      , archiveDistributionSimpleExecutableInstall :: Maybe Text
                        -- directories that will be included in the release's tar archive that should
                        -- be installed with the package
      , archiveDistributionIncludes                :: [Text]
      , archiveDistributionDependencies            :: Dependencies
      , archiveDistributionApplications            :: ReleaseApplicationObject
      }
  | NodeDistribution
      { nodeDistributionRawPackageJson :: Text
      , nodeDistributionDependencies   :: Dependencies
      , nodeDistributionBins           :: ReleaseApplicationObject
      }
  | ExternalDistribution
      { externalDistibutionPackageType    :: ExternalPackageType
      , externalDistibutionApplications   :: ReleaseApplicationObject
      , externalDistibutionInstallCommand :: Maybe Text
      }
  deriving (Show, Generic)

getDependencies ArchiveDistribution{..}  = archiveDistributionDependencies
getDependencies NodeDistribution{..}     = nodeDistributionDependencies
getDependencies ExternalDistribution{..} = Dependencies []

getReleaseApplicationFromNpmJsonVal :: Value -> ReleaseApplicationObject
getReleaseApplicationFromNpmJsonVal (Object o) =
  case HM.lookupDefault emptyObject "bin" o of
    Object binObj -> fromAesonObject binObj
    _             -> ReleaseApplicationObject []
getReleaseApplicationFromNpmJsonVal _ = ReleaseApplicationObject []

getBinsFromRawNpmJson :: Maybe Text -> ReleaseApplicationObject
getBinsFromRawNpmJson Nothing = ReleaseApplicationObject []
getBinsFromRawNpmJson (Just rawPackageJson) =
  let (value :: Maybe Value) =
        decode (L8.fromStrict $ encodeUtf8 rawPackageJson)
   in fromMaybe
        (ReleaseApplicationObject [])
        (getReleaseApplicationFromNpmJsonVal <$> value)


getReleaseApplications :: PackageDistribution -> [ReleaseApplication]
getReleaseApplications (ExternalDistribution _ a _) = unReleaseApplicationObject a
getReleaseApplications (ArchiveDistribution _ _ (Just simpleExe) _ _ _) =
  [ReleaseApplication (last $ T.splitOn "/" simpleExe) (Just simpleExe)]
getReleaseApplications (ArchiveDistribution _ _ _ _ _ a) = unReleaseApplicationObject a
getReleaseApplications (NodeDistribution raw _ (ReleaseApplicationObject alreadyParsedBins)) =
  if (not $ null alreadyParsedBins)
    then alreadyParsedBins
    else unReleaseApplicationObject $ getBinsFromRawNpmJson (Just raw)

instance FromJSON PackageDistribution where
  parseJSON =
    withObject
      "PackageDistribution"
      (\o ->
         if isJust $ HM.lookup "rawPackageJson" o
           then NodeDistribution <$> o .: "rawPackageJson" <*>
                (o .:? "depends" .!= (Dependencies [])) <*>
                (o .:? "bins" .!= ReleaseApplicationObject [])
           else if isJust $ HM.lookup "external" o
                  then ExternalDistribution <$> o .: "external" <*>
                       o .:? "bins" .!= (ReleaseApplicationObject []) <*>
                       o .:? "installCommand"
                  else ArchiveDistribution <$> o .: "platform" <*> o .: "uri" <*>
                       o .:? "simpleExecutableInstall" <*>
                       o .:? "includes" .!= [] <*>
                       o .:? "depends" .!= (Dependencies []) <*>
                       o .:? "bins" .!= ReleaseApplicationObject []
        )

instance ToJSON PackageDistribution where
  toJSON (NodeDistribution r d b) = object ["rawPackageJson" .= r, "depends" .= d, "bins" .= b]
  toJSON (ArchiveDistribution p u s i d b) =
    object
      [ "platform" .= p
      , "uri" .= u
      , "simpleExecutableInstall" .= s
      , "includes" .= i
      , "depends" .= d
      , "bins" .= b
      ]
  toJSON (ExternalDistribution t b c) = object ["external" .= t, "bins" .= b, "installCommand" .= c]

isArchiveDistribution :: PackageDistribution -> Bool
isArchiveDistribution ArchiveDistribution{..} = True
isArchiveDistribution _                       = False

isNodeDistribution :: PackageDistribution -> Bool
isNodeDistribution NodeDistribution{..} = True
isNodeDistribution _                    = False

isExternalDistribution :: PackageDistribution -> Bool
isExternalDistribution ExternalDistribution{..} = True
isExternalDistribution _                        = False

getPlatform :: PackageDistribution -> Platform
getPlatform  NodeDistribution{..}        = AllPlatforms
getPlatform (ExternalDistribution t _ _) = platformForPackageType t
getPlatform  a                           = archiveDistributionPlatform a

instance ToJSON VersionRange where
  toJSON v = String (toText $ prettyShow v)

data Dependency = Dependency
  { dependencyName         :: Text
  , dependencyVersionRange :: VersionRange
  } deriving (Show, Generic, Eq)

printDependency :: Dependency -> Text
printDependency d =
  (dependencyName d) <> "@" <> (toText . prettyShow $ dependencyVersionRange d)

newtype Dependencies = Dependencies { unDependencies :: [Dependency]} deriving (Show, Generic, Eq, Semigroup, Monoid)

instance FromJSON Dependencies where
  parseJSON (Object o) =
    return . Dependencies . filterJustAndUnwrap $
    map
      (\(k, v) ->
         case v of
           String v' -> do
             let parsed = parseVersionRange v'
             either (fail $ "couldn't parse version range: " ++ (toString v')) (Just . Dependency k) parsed
           _         -> Nothing) $
    HM.toList o
  parseJSON (Array items) = mconcat <$> mapM parseJSON (V.toList items)
  parseJSON (String s) = return . Dependencies $ [Dependency s anyVersion]
  parseJSON other = typeMismatch "dependencies should be an object" other

instance ToJSON Dependencies where
  toJSON (Dependencies deps) =
    object $ map (\d -> (dependencyName d) .= (dependencyVersionRange d)) deps

data PackageSpec = PackageSpec {
  name          :: Text,
  version       :: Text,
  author        :: Text,
  copyright     :: Text,
  license       :: Text,
  distributions :: [PackageDistribution]
} deriving (Show, Generic)

instance ToJSON PackageSpec

instance FromJSON PackageSpec where
  parseJSON = withObject "PackageSpec" (\o ->
                            PackageSpec
                              <$> o .: "name"
                              <*> o .: "version"
                              <*> o .: "author"
                              <*> o .: "copyright"
                              <*> o .: "license"
                              <*> o .:? "distributions" .!= []
                            )
