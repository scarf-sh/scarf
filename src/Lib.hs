{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Exception          (throwIO)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Time.Clock.POSIX
import           System.Exit
import           System.IO                  (hClose, hPutStr)
import           System.Process.Typed


data ExecutionResult = ExecutionResult
  { result    :: ExitCode
  , runtimeMS :: Integer
  } deriving (Show)

runProgramWrapped :: FilePath -> IO ExecutionResult
runProgramWrapped f = do
  start <- (round . (* 1000)) `fmap` getPOSIXTime
  exitCode <- runProcess $ shell f
  end <- (round . (* 1000)) `fmap` getPOSIXTime
  return $ ExecutionResult exitCode (end - start)

