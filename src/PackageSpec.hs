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

module PackageSpec where

import           Common

import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Aeson.TH
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Database.Beam.Backend.SQL.Row
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Backend.Types
import           Database.Beam.Postgres
import qualified Dhall                           as Dhall
import           GHC.Generics
import           Lens.Micro.Platform
import           Prelude                         hiding (FilePath, writeFile)

data Platform = MacOS | Linux_i386 | Linux_x86_64 deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance Dhall.Interpret Platform

instance FromBackendRow Postgres Platform where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Platform where
  sqlValueSyntax = autoSqlValueSyntax

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

