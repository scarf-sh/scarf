{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception.Safe    as SE

import           Control.Monad.Reader
import           Data.Maybe
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Network.HTTP.Client       (defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import           Scarf.Common
import           Scarf.Lib
import           Scarf.Types               hiding (command)
import           Servant.Client
import           System.Directory
import           System.Environment
import           System.Exit
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger


data ScarfArgs
  = ScarfInstall
      { pkgName           :: Maybe Text
                 -- , pkgVersion        :: Maybe Text
      , systemPackageFile :: Bool
      }
  | ScarfExecute
      { target :: Text
      , args   :: Text
      , alias  :: Maybe Text
      }
  | ScarfLintPackage
      { packageFile :: FilePath
      }
  | ScarfUploadPackageRelease
      { packageFile :: FilePath
      }
  | ScarfUpgrade
  | ScarfSyncPackageAccess
  | ScarfVersion
  | ScarfFeedback
  deriving (Show)

installInput :: Parser ScarfArgs
installInput = ScarfInstall <$> (optional $ argument str
  (help "Binary, script, etc to install with u")) <*>
  -- (optional $ strOption (long "version" <> help "version bounds of the package to install")) <*>
  (flag False True (long "system-package-file" <> help "install all the packages in the system package file located at ~/.scarf/scarf-package-file.json"))

executeInput :: Parser ScarfArgs
executeInput = ScarfExecute <$> argument str
  (
  metavar "FILENAME"
  <> help "Binary, script, etc to run with u" ) <*>
  strOption (long "args" <> metavar "ARGS" <> help "args to pass to the target program") <*>
  (optional $ strOption (long "alias" <> metavar "ALIAS" <> help "specify an alternative wrapped entry point of a package"))

lintPackageFileInput :: Parser ScarfArgs
lintPackageFileInput = ScarfLintPackage <$> argument str
  (
  metavar "FILENAME"
  <> help "Dhall package file you'd like to validate" )

uploadPackageReleaseInput :: Parser ScarfArgs
uploadPackageReleaseInput = ScarfUploadPackageRelease <$> argument str
  (metavar "FILENAME" <> help "Dhall package file to upload")

feedbackInput :: Parser ScarfArgs
feedbackInput = pure ScarfFeedback

versionInput :: Parser ScarfArgs
versionInput = pure ScarfVersion

upgradeInput :: Parser ScarfArgs
upgradeInput = pure ScarfUpgrade

syncPackageAccessInput :: Parser ScarfArgs
syncPackageAccessInput = pure ScarfSyncPackageAccess

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts description = info (helper <*> opts) $ progDesc description

versionOption :: Parser (a -> a)
versionOption = infoOption (toString scarfCliVersion) (long "version" <> help "Show version")

input :: Parser ScarfArgs
input = subparser $
  command "install" (installInput `withInfo` "Install a package") <>
  command "execute" (executeInput `withInfo` "Runs a scarf-installed executable. You probably don't need to be calling this directly") <>
  command "check-package" (lintPackageFileInput `withInfo` "Check a dhall based scarf package file") <>
  command "sync-access" (syncPackageAccessInput `withInfo` "Sync your local Scarf installation with your package purchases.") <>
  command "upload" (uploadPackageReleaseInput `withInfo` "Create a new release for your package") <>
  command "upgrade" (upgradeInput `withInfo` "Get the lastest version of the Scarf CLI") <>
  command "feedback" (feedbackInput `withInfo` "Send feedback about Scarf")

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
  createDirectoryIfMissing True $ home ++ "/.scarf/log"
  updateGlobalLogger rootLoggerName removeHandler
  h <-
    fileHandler (home ++ "/.scarf/log/scarf.log") INFO >>= \lh ->
      return $
      setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  _ <- updateGlobalLogger rootLoggerName (setLevel INFO . setHandlers [h])
  let config =
        Config
          (toText home)
          (toText <$> apiToken)
          (manager')
          (fromMaybe "https://scarf.sh" baseUrl)
  case options of
    ScarfInstall (Just p) _ -> runReaderT (installProgramWrapped p Nothing) config
    ScarfInstall _ True -> runReaderT (installAll) config
    ScarfInstall _ _ ->
      putStrLn
        "Please specify a package name or use the --system-package-file flag to install from your local system package file." >>
      (exitWith $ ExitFailure 1)
    ScarfExecute f a b -> runReaderT (runProgramWrapped f a b) config >> return ()
    ScarfLintPackage f -> runReaderT (lintPackageFile f) config >> return ()
    ScarfUploadPackageRelease f -> runReaderT (uploadPackageRelease f) config
    ScarfVersion -> putTextLn scarfCliVersion
    ScarfUpgrade -> runReaderT upgradeCli config
    ScarfSyncPackageAccess -> runReaderT syncPackageAccess config
    ScarfFeedback -> runReaderT sendFeedback config
