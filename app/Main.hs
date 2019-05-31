{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception.Safe  as SE
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Semigroup          ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Network.HTTP.Client     (defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           Prelude                 hiding (FilePath)
import           Scarf.Common
import           Scarf.Lib
import           Scarf.Types
import           Servant.Client
import           System.Environment
import           System.Exit

scarfCliVersion :: String
scarfCliVersion = "0.2.0"

data ScarfArgs
  = ScarfInstall { pkgName :: Maybe Text, systemPackageFile :: Bool }
  | ScarfExecute { target :: Text
                 , args   :: Text }
  | ScarfLintPackage { packageFile :: FilePath }
  | ScarfUploadPackageRelease { packageFile :: FilePath }
  | ScarfVersion
  deriving (Show)

installInput :: Parser ScarfArgs
installInput = ScarfInstall <$> (optional $ argument str
  (
  metavar "FILENAME"
  <> help "Binary, script, etc to install with u")) <*>
  (flag False True (long "system-package-file" <> help "install all the packages in the system package file located at ~/.scarf/scarf-package-file.json"))

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
  manager' <- newManager tlsManagerSettings
  let config =
        Config
          (toText home)
          (toText <$> apiToken)
          (manager')
          (fromMaybe "https://scarf.sh" baseUrl)
  case options of
    ScarfInstall (Just p) _ -> runReaderT (installProgramWrapped p) config
    ScarfInstall _ True -> runReaderT (installAll) config
    ScarfInstall _ _ ->
      putStrLn
        "Please specify a package name or use the --system-package-file flag to install from your local system package file." >>
      (exitWith $ ExitFailure 1)
    ScarfExecute f a -> runReaderT (runProgramWrapped f a) config >> return ()
    ScarfLintPackage f ->
      runReaderT (lintPackageFile f) config >> return ()
    ScarfUploadPackageRelease f -> runReaderT (uploadPackageRelease f) config
    ScarfVersion -> putStrLn scarfCliVersion
