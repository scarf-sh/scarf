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
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS8L
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
import qualified Text.Read                  as R

data Platform = MacOS | Linux_x86_64 | AllPlatforms deriving (Show, Eq, Read, Generic)

instance ToJSON Platform

instance FromJSON Platform -- where
  -- parseJSON (String s) = maybe (fail "couldn't parse platform") return (R.readMaybe $ toString s)
  -- parseJSON other = typeMismatch "platform expects a string" other

data PackageDistribution
  = ArchiveDistribution { archiveDistributionPlatform :: Platform
                        -- remote url or local file path to a tar.gz archive of your binary and
                        -- optional data paths https:// or ./
                        , archiveDistributionUri :: Text
                        -- we'll enable signature once the checking is implemented
                        -- signature               :: Maybe Text,
                        , archiveDistributionSimpleExecutableInstall :: Text
                        -- directories that will be included in the release's tar archive that should
                        -- be installed with the package
                        , archiveDistributionIncludes :: [Text]
                        , archiveDistributionDependencies :: Dependencies }
  | NodeDistribution { nodeDistributionRawPackageJson :: Text
                     , nodeDistributionDependencies   :: Dependencies }
  deriving (Show, Generic)

getDependencies ArchiveDistribution{..} = archiveDistributionDependencies
getDependencies NodeDistribution{..}    = nodeDistributionDependencies

instance FromJSON PackageDistribution where
  parseJSON =
    withObject
      "PackageDistribution"
      (\o ->
         if isJust $ HM.lookup "rawPackageJson" o
           then NodeDistribution <$> o .: "rawPackageJson" <*> o .:? "depends" .!= (Dependencies [])
           else ArchiveDistribution <$> o .: "platform" <*> o .: "uri" <*>
                o .: "simpleExecutableInstall" <*>
                o .:? "includes" .!= [] <*>
                o .:? "depends" .!= (Dependencies [])
        )

instance ToJSON PackageDistribution where
  toJSON (NodeDistribution r d) = object ["rawPackageJson" .= r, "depends" .= r]
  toJSON (ArchiveDistribution p u s i d) =
    object
      [ "platform" .= p
      , "uri" .= u
      , "simpleExecutableInstall" .= s
      , "includes" .= i
      , "depends" .= d
      ]

isArchiveDistribution :: PackageDistribution -> Bool
isArchiveDistribution ArchiveDistribution{..} = True
isArchiveDistribution _                       = False

isNodeDistribution :: PackageDistribution -> Bool
isNodeDistribution NodeDistribution{..} = True
isNodeDistribution _                    = False

getPlatform :: PackageDistribution -> Platform
getPlatform  NodeDistribution{..} = AllPlatforms
getPlatform  a                    = archiveDistributionPlatform a

instance ToJSON VersionRange where
  toJSON v = String (toText $ prettyShow v)

data Dependency = Dependency
  { dependencyName         :: Text
  , dependencyVersionRange :: VersionRange
  } deriving (Show, Generic, Eq)

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

scarfLevelDepends NodeDistribution{..} = []
scarfLevelDepends archv                = []

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
