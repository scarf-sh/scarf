{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Scarf.Environment
  ( EnvironmentHandle,
    environmentResourceType,
    ModifyCommand (..),
    modifyEnv,
    enterEnv,
    environmentsNs,
  )
where

import Control.Monad
import Data.Aeson
  ( FromJSON (..),
    Value (String),
    decodeFileStrict,
    encodeFile,
    object,
    withObject,
    (.:),
    (.=),
  )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.UUID as UUID
import Nomia.Name
import Nomia.Namespace
import Paths_scarf
import Scarf.Package
import System.Directory
import System.Environment
import System.Environment.XDG.BaseDir
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempFile)
import System.Process
import Type.Reflection

-- "My Env" is the mutable configured environment where the config file is named
-- in xdg config dirs and the gc root is in xdg cache dirs
--
-- We probably eventually want a reduction here to a more general notion of "mutable configured environment"
-- with the config file and gc root location as arbitrary inputs?
--
-- TODO: Figure out mutating environments in place
-- TODO mutable vs maybe not mutable
-- TODO Is this how we want to pass around the resolver?
data EnvironmentHandle = MyEnvironmentHandle (Maybe Observer) Resolver

environmentResourceType :: ResourceType EnvironmentHandle
environmentResourceType = resourceType . fromJust $ UUID.fromString "36d28982-50a5-49ae-8059-7b17fc3e02ca"

data ModifyCommand
  = AddPackage
  | RemovePackage

modifyEnv :: EnvironmentHandle -> ModifyCommand -> Name -> IO ()
modifyEnv (MyEnvironmentHandle _ _) modCommand name = do
  configPath <- defaultConfigPath
  envSpec <- readEnvSpec configPath

  let newEnvSpec =
        envSpec
          { envSpecPackages = case modCommand of
              AddPackage -> Set.insert name (envSpecPackages envSpec)
              RemovePackage -> Set.delete name (envSpecPackages envSpec)
          }

  createDirectoryIfMissing True (takeDirectory configPath)
  LBS.writeFile configPath (prettyEnvSpec newEnvSpec)

-- This is noreturn, we call exitWith. If we later want to factor that out (and we do),
-- we should also factor out the env variable manipulations to leave this
-- process alone and only impact the child
-- TODO Structured errors...
enterEnv :: EnvironmentHandle -> CreateProcess -> IO ()
enterEnv (MyEnvironmentHandle mObs resolver) enterCommand = do
  EnvSpec {envSpecPackages} <- defaultConfigPath >>= readEnvSpec

  let resolvePackage name =
        resolveName resolver name mObs >>= \case
          Nothing -> error ("couldn't look up " <> show name)
          Just (SomeResolvedName rn) ->
            makeAnomic rn nixyAnomicPackageNameType >>= \case
              Nothing -> error ("couldn't make " <> show name <> " NixyAnomic")
              Just pkgExp -> pure $ nixyAnomicPackageNameToJSON pkgExp
  resolvedPackages <- traverse resolvePackage (Set.toList envSpecPackages)

  (ec, path_, nixStderr) <- withSystemTempFile "scarf-enter.json" $ \tempfile temphandle -> do
    hClose temphandle
    encodeFile tempfile resolvedPackages -- TODO use the handle?
    outlink <- getUserCacheFile "scarf" "env/my-env-root"
    nixexpr <- getDataFileName "data/env.nix"
    readProcessWithExitCode
      "nix-build"
      [nixexpr, "--arg", "packages-file", tempfile, "--out-link", outlink]
      ""

  when (ec /= ExitSuccess) $ do
    hPutStrLn stderr nixStderr
    exitFailure -- TODO error message etc
  let path = init path_ </> "bin" -- Proper trimming etc.
  -- TODO what does "setting path" mean on Windows? Do we even have environments?
  newPATH <-
    lookupEnv "PATH" <&> \case
      Nothing -> path
      Just p -> path ++ searchPathSeparator : p
  setEnv "PATH" newPATH
  withCreateProcess enterCommand $ \_ _ _ child ->
    waitForProcess child >>= exitWith

data EnvSpec = EnvSpec
  { envSpecPackages :: Set Name
  }
  deriving (Show)

instance FromJSON EnvSpec where
  parseJSON = withObject "EnvSpec" $ \o ->
    EnvSpec . Set.fromList
      <$> join . sequence
      <$> (map $ either (\_ -> mzero) pure . parseName (Just defaultPackageNs) "command line") -- TODO package ns depending on spec file
      <$> (o .: "packages")

emptyEnvSpec :: EnvSpec
emptyEnvSpec =
  EnvSpec
    { envSpecPackages = Set.empty
    }

prettyEnvSpec :: EnvSpec -> ByteString
prettyEnvSpec (EnvSpec {..}) = encodePretty json
  where
    json =
      object
        -- TODO Pass config-wide default ns
        [ "packages" .= Set.map (String . printName (Just defaultPackageNs)) envSpecPackages
        ]

readEnvSpec :: FilePath -> IO EnvSpec
readEnvSpec configPath = do
  configExists <- doesFileExist configPath
  if configExists
    then do
      result <- decodeFileStrict configPath
      case result of
        Nothing -> error $ "Could not read environment spec " <> configPath -- TODO proper errors
        Just spec -> pure spec
    else pure emptyEnvSpec

defaultConfigPath :: IO FilePath
defaultConfigPath = getUserConfigFile "scarf" "env/my-env.json"

-- TODO This should be composed and contextual, relative to the user's $HOME and global resolver
data EnvironmentResolvedName = MyEnvironmentName (Maybe Observer) Resolver

instance ResolvedName EnvironmentResolvedName where
  makeAnomic _ _ = pure Nothing
  acquireHandle (MyEnvironmentName mObs resolver) rt = pure $ case eqResourceType rt environmentResourceType of
    Just HRefl -> Just $ MyEnvironmentHandle mObs resolver
    Nothing -> Nothing

-- TODO Move Resolver as ns-ns input to the name
environmentsNs :: Resolver -> Params -> Namespace
environmentsNs resolver =
  noParamsNsFun $
    Namespace
      { resolveInNs = resolve
      }
  where
    resolve (NameId nm) mObs
      | nm == "my-env" = pure . Just . SomeResolvedName $ MyEnvironmentName mObs resolver
      | otherwise = pure Nothing
