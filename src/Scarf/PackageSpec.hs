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
{-# LANGUAGE UndecidableInstances   #-}

module Scarf.PackageSpec where

import           Scarf.Common

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Aeson.TH
import           Data.SemVer
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Dhall               as Dhall
import           GHC.Generics
import           Lens.Micro.Platform
import           Prelude             hiding (FilePath, writeFile)

data Platform = MacOS | Linux_x86_64 deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance Dhall.Interpret Platform

-- no lenses because they don't play nice with dhall yet
data PackageDistribution = PackageDistribution {
  platform                :: Platform,
  -- remote url or local file path to a tar.gz archive of your binary and
  -- optional data paths https:// or ./
  uri                     :: Text,
  signature               :: Maybe Text,
  simpleExecutableInstall :: Maybe Text
} deriving (Show, Generic)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageDistribution"}
  ''PackageDistribution
makeFields ''PackageDistribution

instance Dhall.Interpret PackageDistribution

-- no lenses because they don't play nice with dhall yet
data PackageSpec = PackageSpec {
  name          :: Text,
  version       :: Text,
  distributions :: [PackageDistribution]
                 } deriving (Show, Generic)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageSpec"}
  ''PackageSpec
makeFields ''PackageSpec

instance Dhall.Interpret PackageSpec

