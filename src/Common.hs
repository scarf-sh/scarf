{-# LANGUAGE OverloadedStrings #-}

module Common where

import           Control.Exception.Safe (Exception, MonadThrow, SomeException,
                                         throwM)
import           Data.Char
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Typeable

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

data CliError = CliConnectionError Text | NotFoundError Text deriving (Typeable, Show)

instance Exception CliError

