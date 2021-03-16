{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Options.Applicative
import System.Process
import System.Exit

data EnterOpts = EnterOpts
  { enter_command :: CreateProcess
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

data EnvCommand = Enter EnterOpts

envOptions :: Parser EnvCommand
envOptions = hsubparser
  (command "enter"
    (info (Enter <$> enterOptions) mempty)) -- progdesc?

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
  Env (Enter (EnterOpts { enter_command })) <- execParser optionsInfo
  -- Implicitly running in "my env", which, uh, does nothing
  withCreateProcess enter_command $ \ _ _ _ child ->
    waitForProcess child >>= exitWith
