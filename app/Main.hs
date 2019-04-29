{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Scarf.Common
import           Scarf.Lib
import           Scarf.Types

import qualified Control.Exception.Safe as SE
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.HTTP.Client    (defaultManagerSettings, newManager)
import           Options.Applicative
import           Prelude                hiding (FilePath)
import           Servant.Client
import           System.Environment

scarfCliVersion :: String
scarfCliVersion = "0.1.1"

data ScarfArgs
  = ScarfInstall { file :: FilePath }
  | ScarfExecute { target :: Text
                 , args   :: Text }
  | ScarfLintPackage { packageFile :: FilePath }
  | ScarfUploadPackageRelease { packageFile :: FilePath }
  | ScarfVersion
  deriving (Show)

installInput :: Parser ScarfArgs
installInput = ScarfInstall <$> argument str
  (
  metavar "FILENAME"
  <> help "Binary, script, etc to install with u" )

executeInput :: Parser ScarfArgs
executeInput = ScarfExecute <$> argument str
  (
  metavar "FILENAME"
  <> help "Binary, script, etc to run with u" ) <*>
  strOption (long "args" <> metavar "ARGS" <> help "args to pass to the target program")

lintPackageFileInput :: Parser ScarfArgs
lintPackageFileInput = ScarfLintPackage <$> argument str
  (
  metavar "FILENAME"
  <> help "Dhall package file you'd like to validate" )

uploadPackageReleaseInput :: Parser ScarfArgs
uploadPackageReleaseInput = ScarfUploadPackageRelease <$> argument str
  (metavar "FILENAME" <> help "Dhall package file to upload")

versionInput :: Parser ScarfArgs
versionInput = pure ScarfVersion

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts description = info (helper <*> opts) $ progDesc description

versionOption :: Parser (a -> a)
versionOption = infoOption scarfCliVersion (long "version" <> help "Show version")

input :: Parser ScarfArgs
input = subparser $
  command "install" (installInput `withInfo` "Install a package") <>
  command "execute" (executeInput `withInfo` "Runs a scarf-installed executable. You probably don't need to be calling this directly") <>
  command "check-package" (lintPackageFileInput `withInfo` "Check a dhall based scarf package file") <>
  command "upload" (uploadPackageReleaseInput `withInfo` "Create a new release for your package")

inputParserInfo :: ParserInfo ScarfArgs
inputParserInfo = info (helper <*> versionOption <*> input)
     ( fullDesc
     <> progDesc "Scarf the developer focused package manager"
     <> header "Scarf, the command line wrapper" )

main :: IO ()
main = do
  options <- execParser inputParserInfo
  home <- getEnv "HOME"
  apiToken <- lookupEnv "SCARF_API_TOKEN"
  baseUrl <- lookupEnv "SCARF_BASE_URL"
  manager' <- newManager defaultManagerSettings
  let config = Config (toText home) (toText <$> apiToken) (manager') (fromMaybe "https://scarf.com" baseUrl)
  case options of
    ScarfInstall f   -> runReaderT (installProgramWrapped f) config >> return ()
    ScarfExecute f a -> runReaderT (runProgramWrapped f a) config >> return ()
    ScarfLintPackage f ->
      runReaderT (lintDhallPackageFile f) config >> return ()
    ScarfUploadPackageRelease f -> runReaderT (uploadPackageRelease f) config
    ScarfVersion -> putStrLn scarfCliVersion
