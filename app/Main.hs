{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib

import           Control.Monad.Reader
import           Data.Semigroup       ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Options.Applicative
import           Prelude              hiding (FilePath)
import           System.Environment

data UArgs
  = UInstall { file :: FilePath }
  | UExecute { target :: Text
             , args   :: Text } deriving (Show)

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

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts description = info (helper <*> opts) $ progDesc description

input :: Parser UArgs
input = subparser $
  command "install" (installInput `withInfo` "Install an executable with u") <>
  command "execute" (executeInput `withInfo` "Run a u-installed executable")

inputParserInfo = info (input <**> helper)
      ( fullDesc
     <> progDesc "U helps developer tool developers"
     <> header "U, the command line wrapper" )

main :: IO ()
main = do
  options <- execParser inputParserInfo
  home <- getEnv "HOME"
  let config = Config (toText home)
  case options of
    UInstall f   -> runReaderT (installProgramWrapped f) config
    UExecute f a -> runReaderT (runProgramWrapped f a) config >> return ()

