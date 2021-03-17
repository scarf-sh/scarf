{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Options.Applicative
import System.Process
import System.Exit
import Data.Text
import Development.Placeholders

data Name = Name Text

data AddOpts = AddOpts
  { package :: Name
  }

addOptions :: Parser AddOpts
addOptions = AddOpts . Name <$>
  strArgument
    (  metavar "PKG"
    <> help "the name of the package to add"
    )

data EnterOpts = EnterOpts
  { enterCommand :: CreateProcess
  }

enterOptions :: Parser EnterOpts
enterOptions = EnterOpts <$> (proc <$>
  strArgument
    (  metavar "PROG"
    <> help "program to run in the environment"
    )
  <*>
  many (strArgument
         (  metavar "ARG"
         <> help "arguments to the program to run"
         )))

data EnvCommand
  = Enter EnterOpts
  | Add AddOpts

envOptions :: Parser EnvCommand
envOptions = hsubparser
  ((command "enter"
    (info (Enter <$> enterOptions) mempty))
  <>
  (command "add"
   (info (Add <$> addOptions) mempty)))-- progdesc?

data Command = Env EnvCommand

options :: Parser Command
options = hsubparser
  (command "env"
    (info (Env <$> envOptions) mempty)) -- progdesc?

optionsInfo :: ParserInfo Command
optionsInfo = info (options <**> helper)
  (  fullDesc
  <> progDesc "The Scarf command-line tool"
  )

main :: IO ()
main = do
  Env ecmd <- execParser optionsInfo
  case ecmd of
    Enter (EnterOpts { enterCommand }) ->
      -- Implicitly running in "my env", which, uh, does nothing
      withCreateProcess enterCommand $ \ _ _ _ child ->
        waitForProcess child >>= exitWith
    Add (AddOpts { package }) ->
      $notImplemented
