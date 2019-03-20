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

module Lib where

import           Common
import           PackageSpec

import           Control.Exception          (throwIO)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Crypto.JOSE.JWK
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.IO               hiding (putStrLn)
import           Data.Time.Clock.POSIX
import qualified Dhall                      as Dhall
import           DynFlags
import           GHC.Generics
import           Lens.Micro.Platform
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Prelude                    hiding (FilePath, writeFile)
import           Servant.Auth.Server
import           System.Directory
import           System.Exit
import           System.IO                  (hClose, hPutStr)
import           System.Posix.Files
import           System.Posix.Types
import           System.Process.Typed
import           Text.Printf

type FilePath = Text

delimeter :: Text
delimeter = "----"

toString = T.unpack
toText = T.pack

data Config = Config {
  homeDirectory :: FilePath
                     }

data ExecutionResult = ExecutionResult
  { result    :: ExitCode
  , runtimeMS :: Integer
  , args      :: [Text]
  } deriving (Show)

data CreateUserRequest = CreateUserRequest
  { createUserRequestEmail    :: Text
  , createUserRequestUsername :: Text
  , createUserRequestPassword :: Text
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CreateUserRequest"}
  ''CreateUserRequest
makeFields ''CreateUserRequest

data LoginRequest = LoginRequest
  { loginRequestEmail    :: Text
  , loginRequestPassword :: Text
  }

deriveJSON
  defaultOptions {fieldLabelModifier = makeFieldLabelModfier "loginRequest"}
  ''LoginRequest
makeFields ''LoginRequest

data Session = Session {
  sessionUserId   :: Integer,
  sessionEmail    :: Text,
  sessionUsername :: Text
  } deriving (Generic, Show)

deriveJSON
  defaultOptions {fieldLabelModifier = makeFieldLabelModfier "session"}
  ''Session
deriving instance ToJWT Session
deriving instance FromJWT Session
makeFields ''Session

data CreatePackageCallRequest = CreatePackageCallRequest
  { createPackageCallRequestPackageUuid :: Text
  , createPackageCallRequestExit        :: Integer
  , createPackageCallRequestRunTimeMs   :: Integer
  , createPackageCallRequestArgString   :: Text
  }

deriveJSON
  defaultOptions
  {fieldLabelModifier = makeFieldLabelModfier "CreatePackageCallRequest"}
  ''CreatePackageCallRequest
makeFields ''CreatePackageCallRequest

exitNum :: ExitCode -> Integer
exitNum ExitSuccess     = 0
exitNum (ExitFailure i) = fromIntegral i

runProgramWrapped :: (MonadReader Config m, MonadIO m) => FilePath -> Text -> m ExecutionResult
runProgramWrapped f argString =
  let argsToPass = filter (/= "") (T.splitOn delimeter argString)
      uuid = head $ T.splitOn delimeter f
  in do home <- asks homeDirectory
        start <- liftIO $ (round . (* 1000)) `fmap` getPOSIXTime
        exitCode <-
          runProcess $
          proc (toString $ originalProgram home f) (map toString argsToPass)
        end <- liftIO $ (round . (* 1000)) `fmap` getPOSIXTime
        let runtime = (end - start)
            packageCallToLog =
              CreatePackageCallRequest uuid (exitNum exitCode) runtime argString
        -- TODO(#configuration) - the server should be configurable
        initReq <- liftIO $ parseRequest "http://localhost:9001/package-call"
        let request =
              setRequestBodyJSON packageCallToLog $ initReq {method = "POST"}
        -- TODO - logging
        -- liftIO $ print request
        response <- httpBS request
        -- liftIO $ print response
        return $ ExecutionResult exitCode runtime argsToPass

originalProgram homeFolder fileName = homeFolder <>  "/.u/original/" <> fileName

wrappedProgram homeFolder fileName = homeFolder <> "/.u/bin/" <> fileName

type ExecutableId = Text

installProgramWrapped :: (MonadReader Config m, MonadIO m) => FilePath -> ExecutableId -> m ()
installProgramWrapped f uuid =
  let fileName = last $ T.splitOn "/" f in do
    home <- asks homeDirectory
    let wrappedProgramPath = (toString $ wrappedProgram home fileName)
    liftIO $ copyFile (toString (T.replace "~" home f)) (toString $ originalProgram home $ uuid <> delimeter <> fileName )
    -- TODO(#bug) conflicting filenames breaks stuff
    liftIO $ writeFile wrappedProgramPath  (T.unlines ["#!/bin/bash",
                                                       "function join_by { local d=$1; shift; echo -n \"$1\"; shift; printf \"%s\" \"${@/#/$d}\"; }",
                                                       toText $ printf "arg_string=$(join_by \"%s\" \"$@\")" delimeter,
                                                       toText $ printf "u-exe execute %s%s%s --args \"$arg_string\"" uuid delimeter fileName
                                                                          ])
    liftIO $ setFileMode wrappedProgramPath accessModes
    liftIO $ printf "Installation complete: %s\n" wrappedProgramPath

lintDhallPackageFile :: (MonadReader Config m, MonadIO m) => FilePath -> m ()
lintDhallPackageFile f = do
  home <- asks homeDirectory
  let pathFixed = (T.replace "~" home f)
  (parsedPackage :: PackageSpec) <- liftIO $ Dhall.input Dhall.auto pathFixed
  liftIO . putStrLn $ show parsedPackage




