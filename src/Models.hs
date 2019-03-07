{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Models where

import           Database.Beam
import           Database.Beam.Postgres

import           Control.Monad
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time.Clock
import           Lens.Micro.Platform
import           System.Random

{- =============== Models =============== -}

-- Users --

data UserT f
    = User
    { _userId        :: Columnar f Integer
    , _userEmail     :: Columnar f Text
    , _userUsername  :: Columnar f Text
    , _userPassword  :: Columnar f (Maybe Text)
    , _userAPIToken  :: Columnar f Text
    , _userCreatedAt :: Columnar f UTCTime
    , _userUpdatedAt :: Columnar f (Maybe UTCTime)
    }
    deriving (Generic, Beamable)

makeLenses ''UserT

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Eq (PrimaryKey UserT Identity)
deriving instance Show (PrimaryKey UserT (Nullable Identity))
deriving instance Eq (PrimaryKey UserT (Nullable Identity))

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Integer)
                        deriving Generic
  primaryKey = UserId . _userId

instance Beamable (PrimaryKey UserT)

-- Packages --

data PackageT f = Package
  { _packageId        :: Columnar f Integer
  , _packageUploader  :: PrimaryKey UserT f
  , _packageName      :: Columnar f Text
  , _packageVersion   :: Columnar f Text
  , _packageCreatedAt :: Columnar f UTCTime
  } deriving (Generic, Beamable)

makeLenses ''PackageT

type Package = PackageT Identity
type PackageId = PrimaryKey PackageT Identity

deriving instance Show Package
deriving instance Eq Package
deriving instance Show (PrimaryKey PackageT Identity)
deriving instance Eq (PrimaryKey PackageT Identity)

instance Table PackageT where
  data PrimaryKey PackageT f = PackageId (Columnar f Integer)
                        deriving Generic
  primaryKey = PackageId . _packageId

instance Beamable (PrimaryKey PackageT)

-- PackageEvent --

data PackageEventType
  = PackageInstall
  | PackageUninstall
  | PackageUpdate
  deriving (Eq, Show, Generic)

data PackageEventT f = PackageEvent
  { _packageEventId        :: Columnar f Integer
  , _packageEventUser      :: PrimaryKey UserT (Nullable f)
  , _packageEventPackage   :: PrimaryKey PackageT f
  , _packageEventType      :: Columnar f PackageEventType
  , _packageEventCreatedAt :: Columnar f UTCTime
  } deriving (Generic, Beamable)

makeLenses ''PackageEventT

type PackageEvent = PackageEventT Identity
type PackageEventId = PrimaryKey PackageEventT Identity

deriving instance Show PackageEvent
deriving instance Eq PackageEvent
deriving instance Show (PrimaryKey PackageEventT Identity)
deriving instance Eq (PrimaryKey PackageEventT Identity)

instance Table PackageEventT where
  data PrimaryKey PackageEventT f = PackageEventId (Columnar f Integer)
                        deriving Generic
  primaryKey = PackageEventId . _packageEventId

instance Beamable (PrimaryKey PackageEventT)

-- PackageCall --

data PackageCallT f = PackageCall
  { _packageCallId        :: Columnar f Integer
  , _packageCallPackage   :: PrimaryKey PackageT f
  , _packageCallUser      :: PrimaryKey UserT (Nullable f)
  , _packageCallExit      :: Columnar f Integer
  , _packageCallTimeMs    :: Columnar f Integer
  , _packageCallArgString :: Columnar f Text
  , _packageCallCreatedAt :: Columnar f UTCTime
  } deriving (Generic)
instance Beamable PackageCallT

makeLenses ''PackageCallT

type PackageCall = PackageCallT Identity
type PackageCallId = PrimaryKey PackageCallT Identity

deriving instance Show PackageCall
deriving instance Eq PackageCall
deriving instance Show (PrimaryKey PackageCallT Identity)
deriving instance Eq (PrimaryKey PackageCallT Identity)

instance Table PackageCallT where
  data PrimaryKey PackageCallT f = PackageCallId (Columnar f Integer)
                        deriving Generic
  primaryKey = PackageCallId . _packageCallId

instance Beamable (PrimaryKey PackageCallT)

-- Helpers

alphaNumeric :: String
alphaNumeric = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']

genApiToken :: IO Text
genApiToken =
  let strLength = 25
  in do apiToken <-
          replicateM
            strLength
            (randomRIO (0, length alphaNumeric) >>=
             (\x -> return $ alphaNumeric !! x))
        return $ T.pack apiToken

-- DB Info

data RepoDb f = RepoDb
                      { _repoUsers :: f (TableEntity UserT) }
                        deriving Generic

instance Database be RepoDb

repoDb :: DatabaseSettings be RepoDb
repoDb = defaultDbSettings
