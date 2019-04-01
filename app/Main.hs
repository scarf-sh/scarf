{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Common
import           Lib
import           Types

import           Control.Monad.Reader
import           Data.Maybe
import           Data.Semigroup       ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.HTTP.Client  (defaultManagerSettings, newManager)
import           Options.Applicative
import           Prelude              hiding (FilePath)
import           Servant.Client
import           System.Environment

data UArgs
  = UInstall { file :: FilePath }
  | UExecute { target :: Text
             , args   :: Text }
  | ULintPackage { packageFile :: FilePath  }
  | UUploadPackageRelease { packageFile :: FilePath  }
 deriving (Show)

installInput :: Parser UArgs
installInput = UInstall <$> argument str
  (
  metavar "FILENAME"
  <> help "Binary, script, etc to install with u" )

executeInput :: Parser UArgs
executeInput = UExecute <$> argument str
  (
  metavar "FILENAME"
  <> help "Binary, script, etc to run with u" ) <*>
  strOption (long "args" <> metavar "ARGS" <> help "args to pass to the target program")

lintPackageFileInput :: Parser UArgs
lintPackageFileInput = ULintPackage <$> argument str
  (
  metavar "FILENAME"
  <> help "Dhall package file you'd like to validate" )

uploadPackageReleaseInput :: Parser UArgs
uploadPackageReleaseInput = UUploadPackageRelease <$> argument str
  (metavar "FILENAME" <> help "Dhall package file to upload")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts description = info (helper <*> opts) $ progDesc description

input :: Parser UArgs
input = subparser $
  command "install" (installInput `withInfo` "Install an executable with u") <>
  command "execute" (executeInput `withInfo` "Run a u-installed executable") <>
  command "check-package" (lintPackageFileInput `withInfo` "Check a u dhall based package file") <>
  command "upload" (uploadPackageReleaseInput `withInfo` "Create a new package release")

inputParserInfo = info (input <**> helper)
     ( fullDesc
     <> progDesc "U helps developer tool developers"
     <> header "U, the command line wrapper" )

main :: IO ()
main = do
  options <- execParser inputParserInfo
  home <- getEnv "HOME"
  apiToken <- lookupEnv "U_API_TOKEN"
  manager' <- newManager defaultManagerSettings
  let config = Config (toText home) (toText <$> apiToken) (manager')
  case options of
    -- FIXME: pull the package uuid from somewhere
    UInstall f   -> runReaderT (installProgramWrapped f) config >>= print
    UExecute f a -> runReaderT (runProgramWrapped f a) config >> return ()
    ULintPackage f -> runReaderT (lintDhallPackageFile f) config >> return ()
    UUploadPackageRelease f -> runReaderT (uploadPackageRelease f) config

