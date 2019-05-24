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

import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Aeson.TH
import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified Dhall         as Dhall
import           GHC.Generics
import           Prelude       hiding (FilePath, writeFile)

data Platform = MacOS | Linux_x86_64 | AllPlatforms deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance Dhall.Interpret Platform

-- no lenses because they don't play nice with dhall yet
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
  deriving (Show, Generic, ToJSON, FromJSON)

instance Dhall.Interpret PackageDistribution

isArchiveDistribution :: PackageDistribution -> Bool
isArchiveDistribution ArchiveDistribution{..} = True
isArchiveDistribution _                       = False

isNodeDistribution :: PackageDistribution -> Bool
isNodeDistribution NodeDistribution{..} = True
isNodeDistribution _                    = False

getPlatform :: PackageDistribution -> Platform
getPlatform  NodeDistribution{..} = AllPlatforms
getPlatform  a                    = platform a

-- no lenses because they don't play nice with dhall yet
data PackageSpec = PackageSpec {
  name          :: Text,
  version       :: Text,
  author        :: Text,
  copyright     :: Text,
  license       :: Text,
  distributions :: Maybe [PackageDistribution]
} deriving (Show, Generic, ToJSON, FromJSON)

instance Dhall.Interpret PackageSpec

