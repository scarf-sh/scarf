{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor
import Data.Text hiding (init)
import Development.Placeholders
import Options.Applicative
import Paths_scarf
import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir
import System.Exit
import System.FilePath
import System.Process

data EnvSpec = EnvSpec
  { envSpecPackages :: [Name]
  }

instance FromJSON EnvSpec where
  parseJSON = withObject "EnvSpec" $ \o ->
    EnvSpec
      <$> o .: "packages"

instance ToJSON EnvSpec where
  toJSON EnvSpec {..} =
    object
      [ "packages" .= envSpecPackages
      ]

emptyEnvSpec :: EnvSpec
emptyEnvSpec =
  EnvSpec
    { envSpecPackages = []
    }

prettyEnvSpec :: EnvSpec -> ByteString
prettyEnvSpec = encodePretty

-- "My Env" is the mutable configured environment where the config file is named
-- in xdg config dirs and the gc root is in xdg cache dirs
--
-- We probably eventually want a reduction here to a more general notion of "mutable configured environment"
-- with the config file and gc root location as arbitrary inputs?

-- This is noreturn, we call exitWith. If we later want to factor that out,
-- we should also factor out the env variable manipulations to leave this
-- process alone and only impact the child
enterMyEnv :: CreateProcess -> IO ()
enterMyEnv enterCommand = do
  configPath <- getUserConfigFile "scarf" "env/my-env.json"
  -- TODO TOCTTOU
  hasConfigPath <- doesFileExist configPath
  when hasConfigPath $ do
    outlink <- getUserCacheFile "scarf" "env/my-env-root"
    nixexpr <- getDataFileName "data/env.nix"
    (ec, path_, _) <-
      readProcessWithExitCode
        "nix-build"
        [nixexpr, "--arg", "config-file", "--output", outlink]
        ""
    when (ec /= ExitSuccess) exitFailure -- TODO error message etc
    let path = init path_ </> "bin" -- Proper trimming etc.
    -- TODO what does "setting path" mean on Windows? Do we even have environments?
    newPATH <-
      lookupEnv "PATH" <&> \case
        Nothing -> path
        Just p -> path ++ searchPathSeparator : p
    setEnv "PATH" newPATH
  withCreateProcess enterCommand $ \_ _ _ child ->
    waitForProcess child >>= exitWith

-- | This is implicitly pulled from the Scarf package set for now
data Name = Name {name :: Text}

instance FromJSON Name where
  parseJSON = withText "Text" (pure . Name)

instance ToJSON Name where
  toJSON Name {name} = toJSON name

data AddOpts = AddOpts
  { package :: Name
  }

addOptions :: Parser AddOpts
addOptions =
  AddOpts . Name
    <$> strArgument
      ( metavar "PKG"
          <> help "the name of the package to add"
      )

data EnterOpts = EnterOpts
  { enterCommand :: CreateProcess
  }

enterOptions :: Parser EnterOpts
enterOptions =
  EnterOpts
    <$> ( proc
            <$> strArgument
              ( metavar "PROG"
                  <> help "program to run in the environment"
              )
            <*> many
              ( strArgument
                  ( metavar "ARG"
                      <> help "arguments to the program to run"
                  )
              )
        )

data EnvCommand
  = Enter EnterOpts
  | Add AddOpts

envOptions :: Parser EnvCommand
envOptions =
  hsubparser
    ( ( command
          "enter"
          (info (Enter <$> enterOptions) mempty)
      )
        <> ( command
               "add"
               (info (Add <$> addOptions) mempty) -- progdesc?
           )
    )

data Command = Env EnvCommand

options :: Parser Command
options =
  hsubparser
    ( command
        "env"
        (info (Env <$> envOptions) mempty) -- progdesc?
    )

optionsInfo :: ParserInfo Command
optionsInfo =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc "The Scarf command-line tool"
    )

main :: IO ()
main = do
  Env ecmd <- execParser optionsInfo
  case ecmd of
    Enter (EnterOpts {enterCommand}) ->
      enterMyEnv enterCommand
    Add (AddOpts {package}) ->
      $notImplemented
