{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

module Models where

import           PackageSpec

import           Control.Monad
import           Crypto.KDF.BCrypt
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.ByteString        as BS
import           Data.Pool
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time.Clock
import           Database.Beam
import           Database.Beam.Postgres
import           Lens.Micro.Platform
import           System.Random


{- =============== Models =============== -}

-- Users --

data UserT f
    = User
    { _userId          :: Columnar f Integer
    , _userEmail       :: Columnar f Text
    , _userUsername    :: Columnar f Text
    , _userPassword    :: Columnar f Text
    , _userOauthSource :: Columnar f (Maybe Text)
    , _userApiToken    :: Columnar f Text
    , _userCreatedAt   :: Columnar f UTCTime
    , _userUpdatedAt   :: Columnar f (Maybe UTCTime)
    }
    deriving (Generic, Beamable)

(makeLensesWith abbreviatedFields) ''UserT

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Eq (PrimaryKey UserT (Nullable Identity))
deriving instance Eq (PrimaryKey UserT Identity)
deriving instance Eq User
deriving instance FromJSON (PrimaryKey UserT (Nullable Identity))
deriving instance FromJSON (PrimaryKey UserT Identity)
deriving instance FromJSON User
deriving instance Show (PrimaryKey UserT (Nullable Identity))
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show User
deriving instance ToJSON (PrimaryKey UserT (Nullable Identity))
deriving instance ToJSON (PrimaryKey UserT Identity)
deriving instance ToJSON User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Integer)
                        deriving Generic
  primaryKey = UserId . _userId

instance Beamable (PrimaryKey UserT)

-- Packages --

data PackageT f = Package
  { packageId               :: Columnar f Integer
  , packageUuid             :: Columnar f Text
  , packageOwner            :: PrimaryKey UserT f
  , packageName             :: Columnar f Text
  , packageShortDescription :: Columnar f Text
  , packageLongDescription  :: Columnar (Nullable f) Text
  , packageCreatedAt        :: Columnar f UTCTime
  } deriving (Generic, Beamable)

(makeLensesWith abbreviatedFields) ''PackageT

type Package = PackageT Identity
type PackageId = PrimaryKey PackageT Identity

deriving instance Eq (PrimaryKey PackageT Identity)
deriving instance Eq Package
deriving instance FromJSON (PrimaryKey PackageT (Nullable Identity))
deriving instance FromJSON (PrimaryKey PackageT Identity)
deriving instance FromJSON Package
deriving instance Show (PrimaryKey PackageT Identity)
deriving instance Show Package
deriving instance ToJSON (PrimaryKey PackageT (Nullable Identity))
deriving instance ToJSON (PrimaryKey PackageT Identity)
deriving instance ToJSON Package

instance Table PackageT where
  data PrimaryKey PackageT f = PackageId (Columnar f Text)
                        deriving Generic
  primaryKey = PackageId . packageUuid

instance Beamable (PrimaryKey PackageT)


-- PackageRelease  --

data PackageReleaseT f = PackageRelease
  { packageReleaseId                  :: Columnar f Integer
  , packageReleaseUuid                :: Columnar f Text
  , packageReleasePackage             :: PrimaryKey PackageT f
  , packageReleaseUploader            :: PrimaryKey UserT f
  , packageReleaseVersion             :: Columnar f Text
  , packageReleaseExecutableUrl       :: Columnar f Text
  , packageReleasePlatform            :: Columnar f Platform
  , packageReleaseExecutableSignature :: Columnar (Nullable f) Text
  , packageReleaseCreatedAt           :: Columnar f UTCTime
  } deriving (Generic, Beamable)

(makeLensesWith abbreviatedFields) ''PackageReleaseT

type PackageRelease = PackageReleaseT Identity
type PackageReleaseId = PrimaryKey PackageReleaseT Identity

deriving instance Show PackageRelease
deriving instance Eq PackageRelease
deriving instance Show (PrimaryKey PackageReleaseT Identity)
deriving instance Eq (PrimaryKey PackageReleaseT Identity)

instance Table PackageReleaseT where
  data PrimaryKey PackageReleaseT f = PackageReleaseId (Columnar f Text)
                        deriving Generic
  primaryKey = PackageReleaseId . packageReleaseUuid

instance Beamable (PrimaryKey PackageReleaseT)

-- PackageEvent --

data PackageEventType
  = PackageInstall
  | PackageUninstall
  | PackageUpdate
  deriving (Eq, Show, Generic)

data PackageEventT f = PackageEvent
  { packageEventId             :: Columnar f Integer
  , packageEventUser           :: PrimaryKey UserT (Nullable f)
  , packageEventPackageRelease :: PrimaryKey PackageReleaseT f
  , packageEventType           :: Columnar f PackageEventType
  , packageEventCreatedAt      :: Columnar f UTCTime
  } deriving (Generic, Beamable)

(makeLensesWith abbreviatedFields) ''PackageEventT

type PackageEvent = PackageEventT Identity
type PackageEventId = PrimaryKey PackageEventT Identity

deriving instance Show PackageEvent
deriving instance Eq PackageEvent
deriving instance Show (PrimaryKey PackageEventT Identity)
deriving instance Eq (PrimaryKey PackageEventT Identity)

instance Table PackageEventT where
  data PrimaryKey PackageEventT f = PackageEventId (Columnar f Integer)
                        deriving Generic
  primaryKey = PackageEventId . packageEventId

instance Beamable (PrimaryKey PackageEventT)

-- PackageCall --

data PackageCallT f = PackageCall
  { _packagecallId             :: Columnar f Integer
  , _packagecallPackageRelease :: PrimaryKey PackageReleaseT f
  , _packagecallUser           :: PrimaryKey UserT (Nullable f)
  , _packagecallExit           :: Columnar f Integer
  , _packagecallTimeMs         :: Columnar f Integer
  , _packagecallArgString      :: Columnar f Text
  , _packagecallCreatedAt      :: Columnar f UTCTime
  } deriving (Generic)
instance Beamable PackageCallT

(makeLensesWith abbreviatedFields) ''PackageCallT

type PackageCall = PackageCallT Identity
type PackageCallId = PrimaryKey PackageCallT Identity

deriving instance Show PackageCall
deriving instance Eq PackageCall
deriving instance Show (PrimaryKey PackageCallT Identity)
deriving instance Eq (PrimaryKey PackageCallT Identity)

instance Table PackageCallT where
  data PrimaryKey PackageCallT f = PackageCallId (Columnar f Integer)
                        deriving Generic
  primaryKey = PackageCallId . _packagecallId

instance Beamable (PrimaryKey PackageCallT)

{- =============== Helpers =============== -}

alphaNumeric :: String
alphaNumeric = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']

genApiToken :: IO Text
genApiToken =
  let strLength = 25
  in do apiToken <-
          replicateM
            strLength
            (randomRIO (0, length alphaNumeric - 1) >>=
             (\x -> return $ alphaNumeric !! x))
        return $ T.pack apiToken

{- =============== DB Info =============== -}

data RepoDb f = RepoDb
  { _repoUsers         :: f (TableEntity UserT)
  , _repoPackages      :: f (TableEntity PackageT)
  , _repoPackageEvents :: f (TableEntity PackageEventT)
  , _repoPackageCalls  :: f (TableEntity PackageCallT)
  } deriving (Generic)

instance Database be RepoDb

repoDb :: DatabaseSettings be RepoDb
repoDb = defaultDbSettings

initConnectionPool :: BS.ByteString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe


