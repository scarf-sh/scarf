{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Functor
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import Nomia.Name
import Nomia.Namespace
import Options.Applicative
import Scarf.Environment
import Scarf.Package
import System.IO
import System.Process

data AddOpts = AddOpts
  { package :: Name
  }

data RemoveOpts = RemoveOpts
  { package :: Name
  }

nameReader :: NamespaceId -> ReadM Name
nameReader defaultNs =
  maybeReader $
    either (const Nothing) Just . parseName (Just defaultNs) "CLI" . Text.pack

addOptions :: Parser AddOpts
addOptions =
  AddOpts
    <$> argument
      (nameReader defaultPackageNs)
      ( metavar "PKG"
          <> help "the name of the package to add"
      )

removeOptions :: Parser RemoveOpts
removeOptions =
  RemoveOpts
    <$> argument
      (nameReader defaultPackageNs)
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

data Command = Env Name EnvCommand

showEnvName :: Name -> String
showEnvName = Text.unpack . printName (Just defaultEnvNs)

envName :: Parser Name
envName =
  option
    (nameReader defaultEnvNs)
    ( long "environment"
        <> metavar "ENV"
        <> help "the environment to operate on"
        <> value myEnvName
        <> showDefaultWith showEnvName
    )

options :: Parser Command
options =
  hsubparser
    ( command
        "env"
        (info (Env <$> envName <*> envOptions) mempty) -- progdesc?
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
  let observer = Observer $ \ReductionMessage {..} ->
        -- TODO say where it came from (need mutable state :o)
        hPutStrLn stderr $ "Got reduction. New nsid: " ++ show rm_nsid ++ ". New name: " ++ show rm_nm ++ "."
  Env envn ecmd <- execParser optionsInfo
  SomeResolvedName envrn <-
    resolveName resolver envn (Just observer) <&> \case
      Just srn -> srn
      Nothing -> error ("couldn't resolve env name " ++ showEnvName envn)
  envh <-
    acquireHandle envrn environmentResourceType <&> \case
      Just envh -> envh
      Nothing -> error ("couldn't acquire an environment handle from resolved name at " ++ showEnvName envn)

  case ecmd of
    Enter (EnterOpts {enterCommand}) ->
      enterEnv envh enterCommand
    Add (AddOpts {package}) ->
      modifyEnv envh AddPackage package
    Remove (RemoveOpts {package}) ->
      modifyEnv envh RemovePackage package
