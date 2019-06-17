{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec

import           Scarf.Client
import           Scarf.Common
import           Scarf.Lib
import qualified Scarf.PackageSpec         as PackageSpec
import           Scarf.Types

import           Data.Maybe
import           Data.Monoid
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Distribution.Compat.Graph as Graph
import           Distribution.License
import           Distribution.Parsec.Class
import           Distribution.Pretty
import           Distribution.Version
import           Distribution.Version
import           Lens.Micro.Platform


defaultRelease = PackageRelease {
    packageReleaseUuid                    = ""
  , packageReleaseName                    = ""
  , packageReleaseUploaderName            = ""
  , packageReleaseAuthor                  = ""
  , packageReleaseCopyright               = ""
  , packageReleaseLicense                 = MIT
  , packageReleaseVersion                 = version0
  , packageReleasePlatform                = PackageSpec.Linux_x86_64
  , packageReleaseExecutableUrl           = ""
  , packageReleaseExecutableSignature     = Nothing
  , packageReleaseSimpleExecutableInstall = Nothing
  , packageReleasePackageType             = ArchivePackage
  , packageReleaseNodePackageJson         = Nothing
  , packageReleaseIncludes                = []
  , packageReleaseDepends                 = PackageSpec.Dependencies []
  , packageReleaseCreatedAt               = UTCTime (fromGregorian 2017 2 8) (secondsToDiffTime $ 24 * 3600)
  }

mkRelease _uuid _name _version _depends =
  (uuid .~ _uuid) .
  (name .~ _name) . (version .~ _version) . (depends .~ _depends) $
  defaultRelease


allReleases =
  [ mkRelease "1" "a" version0 mempty
  , mkRelease
      "2"
      "b"
      version0
      (PackageSpec.Dependencies [PackageSpec.Dependency "a" anyVersion])
  ]

main :: IO ()
main = hspec $
  describe "dependencies" $ do
    it "should build a dag" $ do
      (length $ getDepInstallList (allReleases !! 1) allReleases) `shouldBe` 2
      (length $ getDepInstallList (head allReleases) allReleases) `shouldBe` 1



