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

module PackageSpec where

import           Common

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Aeson.TH
import           Data.Text           (Text)
import qualified Dhall               as Dhall
import           GHC.Generics
import           Lens.Micro.Platform
import           Prelude             hiding (FilePath, writeFile)


data Platform = MacOS | X86Linux | X64Linux deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Dhall.Interpret Platform

data PackageDistribution = PackageDistribution {
  platform  :: Platform,
  url       :: Text,
  signature :: Maybe Text
} deriving (Show, Generic)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageDistribution"}
  ''PackageDistribution
makeFields ''PackageDistribution

instance Dhall.Interpret PackageDistribution

data PackageSpec = PackageSpec {
  name             :: Text,
  uploaderUsername :: Text,
  version          :: Text,
  distributions    :: [PackageDistribution]
                 } deriving (Show, Generic)

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "PackageSpec"}
  ''PackageSpec
makeFields ''PackageSpec

instance Dhall.Interpret PackageSpec
