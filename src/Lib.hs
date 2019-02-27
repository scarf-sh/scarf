{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  (
    FilePath,
    Config(..),

    toString,
    toText,
    runProgramWrapped,
    installProgramWrapped
  )
where

import           Control.Exception          (throwIO)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.IO
import           Data.Time.Clock.POSIX
import           DynFlags
import           Prelude                    hiding (FilePath, writeFile)
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

runProgramWrapped :: (MonadReader Config m, MonadIO m) => FilePath -> Text -> m ExecutionResult
runProgramWrapped f argString =
  let argsToPass = T.splitOn delimeter argString in do
    home <- asks homeDirectory
    start <- liftIO $ (round . (* 1000)) `fmap` getPOSIXTime
    exitCode <- runProcess $ proc (toString $ originalProgram home f) (map toString argsToPass)
    end <- liftIO $ (round . (* 1000)) `fmap` getPOSIXTime
    return $ ExecutionResult exitCode (end - start) argsToPass

originalProgram homeFolder fileName = homeFolder <>  "/.u/original/" <> fileName

wrappedProgram homeFolder fileName = homeFolder <> "/.u/bin/" <> fileName

installProgramWrapped :: (MonadReader Config m, MonadIO m) => FilePath -> m ()
installProgramWrapped f =
  let fileName = last $ T.splitOn "/" f in do
    home <- asks homeDirectory
    let wrappedProgramPath = (toString $ wrappedProgram home fileName)
    liftIO $ copyFile (toString (T.replace "~" home f)) (toString $ originalProgram home fileName)
    -- TODO(#bug) conflicting filenames breaks stuff
    liftIO $ writeFile wrappedProgramPath  (T.unlines ["#!/bin/bash",
                                                       "function join_by { local d=$1; shift; echo -n \"$1\"; shift; printf \"%s\" \"${@/#/$d}\"; }",
                                                       toText $ printf "arg_string=$(join_by \"%s\" \"$@\")" delimeter,
                                                       toText $ printf "u-exe execute %s --args \"$arg_string\"" fileName
                                                                          ])
    liftIO $ setFileMode wrappedProgramPath accessModes
