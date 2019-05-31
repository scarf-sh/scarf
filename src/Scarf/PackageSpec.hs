{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

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
import           GHC.Generics
import           Prelude                    hiding (FilePath, writeFile)

data Platform = MacOS | Linux_x86_64 | AllPlatforms deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

data PackageDistribution
  = ArchiveDistribution { platform                :: Platform
                        -- remote url or local file path to a tar.gz archive of your binary and
                        -- optional data paths https:// or ./
                        , uri                     :: Text
                        -- we'll enable signature once the checking is implemented
                        -- signature               :: Maybe Text,
                        , simpleExecutableInstall :: Text
                        -- directories that will be included in the release's tar archive that should
                        -- be installed with the package
                        , includes                :: [Text] }
  | NodeDistribution {rawPackageJson :: Text}
  deriving (Show, Generic)

instance FromJSON PackageDistribution where
  parseJSON =
    withObject
      "PackageDistribution"
      (\o ->
         if isJust $ HM.lookup "rawPackageJson" o
           then NodeDistribution <$> o .: "rawPackageJson"
           else ArchiveDistribution <$> o .: "platform" <*> o .: "uri" <*>
                o .: "simpleExecutableInstall" <*>
                o .:? "includes" .!= [])

instance ToJSON PackageDistribution where
  toJSON (NodeDistribution r) = object ["rawPackageJson" .= r]
  toJSON (ArchiveDistribution p u s i) =
    object
      [ "platform" .= p
      , "uri" .= u
      , "simpleExecutableInstall" .= s
      , "includes" .= i
      ]

isArchiveDistribution :: PackageDistribution -> Bool
isArchiveDistribution ArchiveDistribution{..} = True
isArchiveDistribution _                       = False

isNodeDistribution :: PackageDistribution -> Bool
isNodeDistribution NodeDistribution{..} = True
isNodeDistribution _                    = False

getPlatform :: PackageDistribution -> Platform
getPlatform  NodeDistribution{..} = AllPlatforms
getPlatform  a                    = platform a

data PackageSpec = PackageSpec {
  name          :: Text,
  version       :: Text,
  author        :: Text,
  copyright     :: Text,
  license       :: Text,
  distributions :: [PackageDistribution]
} deriving (Show, Generic, ToJSON)

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
