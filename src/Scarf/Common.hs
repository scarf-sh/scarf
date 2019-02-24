{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Scarf.Common where

import           Control.Exception.Safe (Exception, MonadThrow, SomeException,
                                         throwM)
import           Data.Aeson
import           Data.Char
import           Data.Maybe
import           Data.SemVer
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Typeable
import           GHC.Generics

makeFieldLabelModfier :: String -> String -> String
makeFieldLabelModfier typeName = lowerFirst . (drop $ length typeName)
  where
    lowerFirst :: String -> String
    lowerFirst s =
      if null s
        then s
        else (toLower $ head s) : tail s

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

type FilePath = Text
type ExecutableId = Text
type Username = Text
type PackageName = Text

delimeter :: Text
delimeter = "----"

toString = T.unpack
toText = T.pack

data CliError = CliConnectionError Text | NotFoundError Text | NoCredentialsError | UnknownError Text deriving (Typeable, Show)

instance Exception CliError


-- orphans

instance FromJSON Version where
  parseJSON (String s) = either fail return $ Data.SemVer.fromText s
  parseJSON _          = fail "wrong type"

instance ToJSON Version where
  toJSON versionString = String (Data.SemVer.toText versionString)

getJusts :: [Maybe a] -> [a]
getJusts = (map fromJust) . (filter isJust)
