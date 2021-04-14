{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import Nomia.Name
import Nomia.Namespace
import Options.Applicative
import Scarf.Environment
import Scarf.Package
import System.Process

data AddOpts = AddOpts
  { package :: Name
  }

data RemoveOpts = RemoveOpts
  { package :: Name
  }

packageReader :: ReadM Name
packageReader =
  maybeReader $
    either (const Nothing) Just . parseName (Just defaultPackageNs) "CLI" . Text.pack

addOptions :: Parser AddOpts
addOptions =
  AddOpts
    <$> argument
      packageReader
      ( metavar "PKG"
          <> help "the name of the package to add"
      )

removeOptions :: Parser RemoveOpts
removeOptions =
  RemoveOpts
    <$> argument
      packageReader
      ( metavar "PKG"
          <> help "the name of the package to remove"
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
  | Remove RemoveOpts

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
        <> ( command
               "remove"
               (info (Remove <$> removeOptions) mempty) -- progdesc?
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

-- TODO Make this configurable/extensible
resolver :: Resolver
resolver =
  Resolver $
    Map.fromList
      [ ("scarf-pkgset", scarfPkgset),
        ("nixpkgs", nixpkgsPkgset),
        ("environments", environmentsNs resolver)
      ]

main :: IO ()
main = do
  -- TODO Pass observer through from here
  Just (SomeResolvedName envrn) <- resolveName resolver "environments:my-env" Nothing
  Just envh <- acquireHandle envrn environmentResourceType
  Env ecmd <- execParser optionsInfo
  case ecmd of
    Enter (EnterOpts {enterCommand}) ->
      enterEnv envh enterCommand
    Add (AddOpts {package}) ->
      modifyEnv envh AddPackage package
    Remove (RemoveOpts {package}) ->
      modifyEnv envh RemovePackage package
