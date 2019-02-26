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
import           System.Process.Typed

type FilePath = Text

toString = T.unpack
toText = T.pack

data Config = Config {
  homeDirectory :: FilePath
                     }

data ExecutionResult = ExecutionResult
  { result    :: ExitCode
  , runtimeMS :: Integer
  } deriving (Show)

runProgramWrapped :: FilePath -> IO ExecutionResult
runProgramWrapped f = do
  start <- (round . (* 1000)) `fmap` getPOSIXTime
  exitCode <- runProcess $ shell (T.unpack f)
  end <- (round . (* 1000)) `fmap` getPOSIXTime
  return $ ExecutionResult exitCode (end - start)

originalProgram homeFolder fileName = homeFolder <>  "/.u/original/" <> fileName
wrappedProgram homeFolder fileName = homeFolder <> "/.u/bin/" <> fileName

installProgramWrapped :: (MonadReader Config m, MonadIO m) => FilePath -> m ()
installProgramWrapped f =
  let fileName = last $ T.splitOn "/" f in do
    home <- homeDirectory <$> ask
    liftIO $ copyFile (toString (T.replace "~" home f)) (toString $ originalProgram home fileName)
    -- TODO(#bug) conflicting filenames breaks stuff
    liftIO $ writeFile (toString $ wrappedProgram home fileName) (T.unlines ["#!/bin/bash",
                                                             "args=\"'$*'\"",
                                                             "./u-exe --file " <> (originalProgram home fileName) <> " $args"
                                                                          ])
