{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}

{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Scarf.Lib where

import           Scarf.Client
import           Scarf.Common
import qualified Scarf.PackageSpec                     as PackageSpec
import           Scarf.Types

import qualified Codec.Archive.Tar                     as Tar
import           Codec.Archive.Zip
import qualified Codec.Compression.GZip                as GZ
import           Control.Applicative                   ((<|>))
import qualified Control.Exception                     as UnsafeException
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Crypto.Hash
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty              as AesonPretty
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8            as L8
import           Data.Either
import qualified Data.HashMap.Strict                   as HM
import qualified Data.List                             as List
import           Data.Maybe
import qualified Data.Ord
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding
import           Data.Text.IO                          hiding (putStrLn)
import qualified Data.Text.IO                          as TIO
import           Data.Time.Clock.POSIX
import qualified Data.UUID                             as UUID
import qualified Data.UUID.V4                          as UUID4
import qualified Data.Yaml                             as Yaml
import           Distribution.Parsec.Class
import           Distribution.Pretty
import           Distribution.Text                     (simpleParse)
import           Distribution.Types.VersionRange
import           Lens.Micro.Platform
import           Network.HTTP.Client
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Prelude                               hiding (FilePath,
                                                        writeFile)
import           Servant.Client
import qualified Servant.Client.Core                   as ServantClientCore
import           System.Directory
import           System.Exit
import           System.Info
import           System.IO                             (BufferMode (..), hFlush,
                                                        hSetBuffering, stderr,
                                                        stdin, stdout)
import           System.Log.Logger
import           System.Posix.Files
import           System.Process.Typed
import           Text.Pretty.Simple
import           Text.Printf
import           Text.Read


scarfCliVersion :: Text
scarfCliVersion = "0.10.0"

exitNum :: ExitCode -> Integer
exitNum ExitSuccess     = 0
exitNum (ExitFailure i) = fromIntegral i

type ScarfContext m = (MonadReader Config m, MonadIO m, MonadThrow m, MonadCatch m)
type IOConfigContext m = (MonadReader Config m, MonadIO m)

ifDebug :: (IOConfigContext ma) => IO a -> ma ()
ifDebug action = do
  isDebug <- asks cliDebug
  if isDebug then do
    _ <- liftIO action
    return ()
  else return ()

runProgramWrapped :: (ScarfContext m) => FilePath -> Text -> Maybe Text -> m ExecutionResult
runProgramWrapped f argString maybeAlias =
  let argsToPass =
        filter (/= "") $ splitArgTokens (T.splitOn delimeter argString)
      safeArgString = redactArguments argsToPass
      uuid = head $ T.splitOn delimeter f
   in do home <- asks homeDirectory
         maybeToken <- asks userApiToken
         base <- asks backendBaseUrl
         sysPkgFile <- readSysPackageFile
         pkgEntry <- getPackageEntryForUuid sysPkgFile f maybeAlias
         let tier =
               maybe FreeTier snd $
               ((List.find (\el -> (fst el) == uuid)) =<<
                (sysPkgFile ^. packageAccess))
         pkgType <-
           maybe (throwM $ PackageNotInstalled) return (pkgEntry ^. packageType)
         let nodeEntryPoint = pkgEntry ^. target
         invocation <-
           (case pkgType of
              NodePackage -> return (toString ("node"))
              ArchivePackage ->
                return (toString (originalProgram home f <> "/" <> f))
              ExternalPackage ->
                if (isJust $ pkgEntry ^. target)
                  then return (toString . fromJust $ pkgEntry ^. target)
                  else do
                    targetsInPath <-
                      liftIO $
                      findExecutables
                        (toString $ fromMaybe (pkgEntry ^. name) maybeAlias)
                    let firstMatch =
                          safeHead $
                          List.filter
                            (\t -> not $ ".scarf/bin" `T.isInfixOf` (toText t))
                            targetsInPath
                    (firstMatch `orThrow`
                     (NotFoundError "Couldn't find a target in your path")))
         let args =
               case pkgType of
                 NodePackage ->
                   map toString $
                   (originalProgram home f <> "/" <> fromJust nodeEntryPoint) :
                   argsToPass
                 ArchivePackage -> (map toString argsToPass)
                 ExternalPackage -> (map toString argsToPass)
         start <- liftIO $ (round . (* 1000)) `fmap` getPOSIXTime
         exitCode <-
           liftIO $
           UnsafeException.catch
             (runProcess $ proc invocation args)
             (\(err :: UnsafeException.SomeException) -> do
                print err
                return (ExitFailure (-1)))
         end <- liftIO $ (round . (* 1000)) `fmap` getPOSIXTime
         let runtime = end - start
             packageCallToLog =
               CreatePackageCallRequest
                 uuid
                 (exitNum exitCode)
                 runtime
                 (T.intercalate delimeter safeArgString)
                 (pkgEntry ^. alias)
         if (tier /= PrivateTier)
           then do
             initReq <- liftIO $ parseRequest $ base ++ "/package-call"
             let request =
                   (if isJust maybeToken
                      then setRequestBasicAuth
                             "n/a"
                             (encodeUtf8 $ fromJust maybeToken)
                      else Prelude.id) $
                   setRequestBodyJSON packageCallToLog $
                   initReq {method = "POST"}
             liftIO $
               UnsafeException.catch
                 (void $ httpBS request)
                 (\(err :: UnsafeException.SomeException) ->
                    void $
                    warningM "Scarf" $ "Couldn't log package call: " <> show err)
           else liftIO $
                infoM "Scarf" "private tier package, skipping stat call"
         return $ ExecutionResult exitCode (fromIntegral runtime) safeArgString

uninstallPackage :: ScarfContext m => Text -> m ()
uninstallPackage packageName = do
  sysPkgFile <- readSysPackageFile
  let entriesToRemove = getPackageEntriesForName sysPkgFile packageName
  if null entriesToRemove then do
    liftIO $ putStrLn "Nothing to remove"
  else do
    mapM_ (removeUserInstallation sysPkgFile) entriesToRemove
    liftIO . putStrLn $ toString (packageName <> " uninstalled")

removeUserInstallation :: ScarfContext m => UserState -> UserInstallation -> m ()
removeUserInstallation state@UserState {userStateDepends} installation = do
  ifDebug . pPrint $ "Deleting: " ++ (show installation)
  home <- asks homeDirectory
  sudo <- asks useSudo
  liftIO . removePathForcibly . toString $
    wrappedProgram home $
    fromMaybe (installation ^. name) (installation ^. alias)
  let maybeExternalPkgType = (installation ^. externalPackageType)
  mapM_
    (\externalPkgType -> do
        let (processEntry, args) =
              externalManagerUninstallProcAndArgs
                externalPkgType
                (installation ^. name)
                sudo
        exitCode <- liftIO . runProcess $ proc processEntry args
        liftIO $ print exitCode
        case exitCode of
          ExitSuccess   -> return ()
          ExitFailure e -> throwM $ ExternalInstallFailed e) maybeExternalPkgType
  mapM_
    (\entries -> do
       let newEntries = List.delete installation entries
       writePackageFile $ state {userStateDepends = Just newEntries})
    userStateDepends

syncPackageAccess :: (ScarfContext m) => m ()
syncPackageAccess = do
  base <- asks backendBaseUrl
  request <-
    buildRequestWithTokenAuth
      (toText base <> "/package-access")
      "GET"
  response <- httpJSON request
  if getResponseStatusCode response == 200
    then do
      pkgFile <- readSysPackageFile
      let (respBody :: SyncPackageAccessResponse) = getResponseBody response
      writePackageFile $ pkgFile & packageAccess ?~ (respBody ^. accessList)
      liftIO $ putStrLn "Package access synced"
      return ()
    else throwM $
         UnknownError
           ("[" <> (toText . show $ getResponseStatus response) <> "] Message: " <>
            (toText . show $ response))

getPackageTypeForUuid :: (ScarfContext m) => UserState -> Text -> m PackageType
getPackageTypeForUuid (UserState Nothing _) uuid =
  throwM $
  UserStateCorrupt $
  "Couldn't find installation entry in package file. Please try reinstalling. Package: " <>
  uuid
getPackageTypeForUuid (UserState (Just installs) _) thisUuid =
  let entry =
        List.find (\i -> (fromMaybe "baduuid" $ i ^. uuid) == thisUuid) installs
   in maybe
        (getPackageTypeForUuid (UserState Nothing Nothing) thisUuid)
        (\e -> return $ fromMaybe ArchivePackage $ e ^. packageType)
        entry

getPackageEntryForUuid :: (ScarfContext m) => UserState -> Text -> Maybe Text -> m UserInstallation
getPackageEntryForUuid (UserState Nothing _) thisUuid maybeAlias =
  throwM $
  UserStateCorrupt $
  "Couldn't find installation entry in package file. Please try reinstalling. Package: " <>
  thisUuid
getPackageEntryForUuid (UserState (Just installs) _) thisUuid maybeAlias =
  let entry =
        List.find
          (\i ->
             (fromMaybe "baduuid" $ i ^. uuid) == thisUuid &&
             ((isNothing maybeAlias) || ((i ^. alias) == maybeAlias)))
          installs
   in maybe
        (getPackageEntryForUuid (UserState Nothing Nothing) thisUuid maybeAlias)
        (return)
        entry

getPackageEntriesForName :: UserState -> Text -> [UserInstallation]
getPackageEntriesForName UserState{userStateDepends} packageName =
  filter (\installation -> installation ^. name == packageName) $ fromMaybe [] userStateDepends

splitOnFirst :: Text -> Text -> [Text]
splitOnFirst needle haystack =
  let tokens = T.splitOn needle haystack in
    (head tokens) : [T.intercalate needle (tail tokens)]

-- Given a list of args, split `-flag=value` pairs into distinct tokens
--
-- > splitArgTokens ["--arg=val", "--arg2", "another-val"]
-- > ["--arg", "val", "--arg2", "another-val"]
splitArgTokens :: [Text] -> [Text]
splitArgTokens =
  concatMap (\arg -> if "-" `T.isPrefixOf` arg
        then splitOnFirst "=" arg
        else [arg])

-- This function will get more strict over time as we develop a better notion of
-- what's sensitive. An arg is marked as sensitive if it's:
-- * a directory
-- * a uri
-- * longer than a normal subcommand (for a currently arbitrary choice for that length cutoff)
isSensitiveArg :: Text -> Bool
isSensitiveArg arg =
  -- '/' will eliminate both uri's and directories
  "/" `T.isInfixOf` arg || (T.length arg > 25)

redactArguments :: [Text] -> [Text]
redactArguments argList =
  foldl
    (\tokens currentArg ->
       if ((("-" `T.isPrefixOf` (fromMaybe "" $ safeLast tokens)) &&
            (not $ "-" `T.isPrefixOf` currentArg)) ||
           isSensitiveArg currentArg)
         then tokens ++ ["<REDACTED_ARG>"]
         else tokens ++ [currentArg])
    []
    argList

directoryName :: FilePath -> FilePath
directoryName f =
  let tokens = T.splitOn "/" f in
    T.intercalate "/" $ take (max 1 (length tokens - 1)) tokens

-- FIXME the type of this function is bad
buildRequestWithTokenAuth ::
     (MonadReader Config m, MonadIO m) => Text -> Text -> m Request
buildRequestWithTokenAuth url httpMethod = do
  maybeToken <- asks userApiToken
  initReq <- liftIO . parseRequest $ toString url
  return $ (if isJust maybeToken
      then setRequestBasicAuth "n/a" (encodeUtf8 $ fromJust maybeToken)
      else Prelude.id) (initReq {method = encodeUtf8 httpMethod})

upgradeCli :: (ScarfContext m) => m ()
upgradeCli = do
  home <- asks homeDirectory
  base <- asks backendBaseUrl
  manager' <- asks httpManager
  parsedBaseUrl <- parseBaseUrl base
  response <-
    liftIO $
    runClientM askGetCurrentCliVersion (mkClientEnv manager' parsedBaseUrl)
  case response of
    (Left err) -> throwM . UnknownError . toText . show $ err
    (Right (CliVersionResponse latestVersion)) ->
      liftIO $
      if latestVersion == scarfCliVersion
        then putStrLn "Scarf is up to date!"
        else do
          let platformString =
                if os == "darwin"
                  then "mac"
                  else "linux"
              archiveName = "/tmp/scarf-latest.tar.gz"
              extractToFolder = "/tmp/scarf-latest"
              downloadUrl =
                "https://s3.us-west-2.amazonaws.com/scarf-sh/downloads/scarf/latest/scarf-" <>
                latestVersion <>
                "-" <>
                platformString <>
                ".tar.gz"
              scarfBin = toString $ home <> "/.scarf/bin/scarf"
          putStrLn "Getting the latest Scarf version. Just a moment..."
          downloadArchive downloadUrl archiveName
          Tar.unpack extractToFolder . Tar.read . GZ.decompress =<<
            L8.readFile (toString archiveName)
          copyFile (extractToFolder <> "/scarf") scarfBin
          setPermissions
            scarfBin
            (emptyPermissions
               { readable = True
               , executable = True
               , searchable = True
               , writable = True
               })
          putStrLn "Upgrade complete!"
  return ()

uploadPackageRelease :: (ScarfContext m) => FilePath -> m ()
uploadPackageRelease f = do
  (token :: Text) <-
    maybe (throwM NoCredentialsError) return =<< asks userApiToken
  httpMgr <- asks httpManager
  base <- asks backendBaseUrl
  spec <- getUnvalidatedPackageFile f
  validatedSpec <- lintPackageFile f
  liftIO $ putStrLn "Uploading release"
  pPrint spec
  -- create archive if we have a node distribution
  thisDirectory <- liftIO $ listDirectory (toString $ directoryName f)
  when
    (isJust $
     List.find (PackageSpec.isNodeDistribution) $ validatedSpec ^. distributions)
    (do let filteredItems =
              [ i
              | i <- thisDirectory
              , i `notElem` ["node_modules", ".git", ".gitignore"]
              ]
        liftIO $
          L8.writeFile "/tmp/scarf-node-archive.tar.gz" .
          GZ.compress . Tar.write =<<
          Tar.pack "." filteredItems)
  -- upload any archives
  let distrubutionsToUpload =
        filter
          (\d ->
             (PackageSpec.isNodeDistribution d) ||
             ((not $ PackageSpec.isExternalDistribution d) &&
              (not . isRemoteUrl $ PackageSpec.archiveDistributionUri d))) $
        validatedSpec ^. distributions
      archiveFileParts =
        map
          (\dist ->
             case dist of
               PackageSpec.ArchiveDistribution {..} ->
                 partFileSource
                   ("archive-" <>
                    (toText . show $
                     PackageSpec.archiveDistributionPlatform dist))
                   (toString $ PackageSpec.archiveDistributionUri dist)
               PackageSpec.NodeDistribution {..} ->
                 partFileSource
                   "archive-allplatforms"
                   "/tmp/scarf-node-archive.tar.gz")
          distrubutionsToUpload
  initReq <- liftIO $ parseRequest $ base ++ "/package/release"
  let request =
        setRequestBasicAuth "n/a" (encodeUtf8 token) $ initReq {method = "POST"}
  formified <-
    formDataBody
      ([ partFileRequestBody "spec" "spec.json" $
         RequestBodyBS (L8.toStrict $ encode spec)
       ] ++
       archiveFileParts)
      request
  response <- liftIO $ Network.HTTP.Client.httpLbs formified httpMgr
  if getResponseStatusCode response == 200
    then void $ pPrint "Upload complete!"
    else void $
         liftIO . putStrLn $
         ("[" <> (show $ getResponseStatusCode response) <> "] " <>
          (toString . decodeUtf8 . L8.toStrict $ getResponseBody response))

isRemoteUrl u =
  ("http://" `T.isPrefixOf` u) || ("https://" `T.isPrefixOf` u)

originalProgram homeFolder fileName = homeFolder <>  "/.scarf/original/" <> fileName

wrappedProgram homeFolder fileName = homeFolder <> "/.scarf/bin/" <> fileName

sysPackageFilePath homeFolder = homeFolder <> "/.scarf/scarf-package.json"

setUpScarfDirs ::
     (MonadReader Config m, MonadIO m, MonadThrow m) => m ()
setUpScarfDirs = do
  home <- asks homeDirectory
  liftIO $ createDirectoryIfMissing True (toString $ originalProgram home "")
  liftIO $ createDirectoryIfMissing True (toString $ wrappedProgram home "")

installAll :: (ScarfContext m) => m ()
installAll = do
  userState <- readSysPackageFile
  mapM_ (\(i :: UserInstallation) -> installProgramWrapped (i ^. name) (i ^. version)) (getDependencies userState)

readSysPackageFile :: (ScarfContext m) => m UserState
readSysPackageFile = do
  home <- asks homeDirectory
  let userPackageFile = toString $ sysPackageFilePath home
  packageFileExists <- liftIO $ doesFileExist userPackageFile
  decodedPackageFile <-
    if packageFileExists
      then liftIO $ eitherDecode <$> L8.readFile userPackageFile
      else return . Right $ UserState Nothing Nothing
  when
    (isLeft decodedPackageFile)
    (throwM . UserStateCorrupt $
     "Couldn't read package file. It might be an old version that is no longer supported. Try deleting `~/.scarf/scarf-package.json` and re-installing your package or downgrade to an older version of Scarf ." <>
     (toText $ show decodedPackageFile))
  return $ fromRight (UserState Nothing Nothing) decodedPackageFile

getDepInstallList :: (ScarfContext m) => PackageRelease -> [PackageRelease] -> m [PackageRelease]
getDepInstallList release allReleases =
  let d@(PackageSpec.Dependencies deps) = (release ^. depends)
   in do results <-
           concatMapM
             (\dep -> do
                latestReleasesForDep <-
                  (getLatestReleasesForDependency dep allReleases) `orThrowM`
                  (NotFoundError $
                   "No release for dependency: " <>
                   (PackageSpec.dependencyName dep))
                getDepInstallList latestReleasesForDep allReleases)
             deps
         return $
           List.nubBy
             (\r1 r2 -> r1 ^. name == r2 ^. name)
             (results ++ [release])

getLatestReleaseByName :: ScarfContext m => [PackageRelease] -> Text -> m (Maybe PackageRelease)
getLatestReleaseByName allPkgs query = do
  filtered <- filterM (\r -> do
                          plan <- selectInstallPlan (listInstallPlans r)
                          return $ (r ^. name == query) && (isJust plan)
                      ) allPkgs
  return . safeLast $
    List.sortOn (^. version) filtered

getLatestReleasesForDependency :: ScarfContext m =>
     PackageSpec.Dependency -> [PackageRelease] -> m (Maybe PackageRelease)
getLatestReleasesForDependency dep allReleases =
  getLatestReleaseByName allReleases (PackageSpec.dependencyName dep)

runGetPackageDetails :: (ScarfContext m) => Text -> m PackageDetails
runGetPackageDetails pkgName = do
  base <- asks backendBaseUrl
  manager' <- asks httpManager
  parsedBaseUrl <- parseBaseUrl base
  result <-
    liftIO $
    runClientM
      (askGetPackageDetails pkgName)
      (mkClientEnv manager' parsedBaseUrl)
  either (throwM . makeCliError) (return) result

-- Currently this just checks if you have the latest version, and you'll
-- reinstall otherwise. We can change this behavior once we have proper
-- dependenc version checking
isReleaseInstalled :: UserState -> PackageRelease -> Bool
isReleaseInstalled state rls =
  let deps = getDependencies state
   in isJust $
      List.find
        (\dep ->
           ((dep ^. name) == (rls ^. name)) &&
           (dep ^. uuid) == (Just $ rls ^. uuid))
        deps

installProgramWrapped ::
     (ScarfContext m) => Text -> Maybe Text -> m ()
installProgramWrapped pkgName maybeVersion= do
  (maybeEitherVersion :: Maybe (Either String VersionRange)) <-
    return $ parseVersionRange <$> maybeVersion
  validatedVersion <-
    case maybeEitherVersion of
      Nothing           -> return Nothing
      (Just (Left err)) -> throwM . MalformedVersion $ toText err
      (Just (Right v))  -> return $ Just v
  home <- asks homeDirectory
  base <- asks backendBaseUrl
  _ <- setUpScarfDirs
  manager' <- asks httpManager
  parsedBaseUrl <- parseBaseUrl base
  liftIO . putStrLn . toString $ "Installing " <> pkgName
  decodedPackageFile <- readSysPackageFile
  _details <-
    liftIO $
    runClientM
      (askGetPackageDetails pkgName)
      (mkClientEnv manager' parsedBaseUrl)
  case _details of
    Left servantErr -> throwM $ makeCliError servantErr
    Right details -> do
      maybeRelease <-
            latestRelease
              hostPlatform
              details
              (fromMaybe anyVersion validatedVersion)
      ifDebug $ pPrint details
      when (isNothing maybeRelease) $ throwM $ NotFoundError "No release found"
      let releaseToInstall = fromJust maybeRelease
      -- TODO(#optimize) we don't need to be fetching the index and the details, just the index will work
      indexResponse <-
        liftIO $
        runClientM
          (askGetPackageIndex $ LatestPackageIndexRequest hostPlatform)
          (mkClientEnv manager' parsedBaseUrl)
      LatestPackageIndex index <-
        either
          (const . throwM $ CliConnectionError "Couldn't get package index")
          return
          indexResponse
      liftIO $ putStrLn "Generating dependency list"
      fullDependencyList <- getDepInstallList releaseToInstall index
      liftIO . putStrLn $
        printf
          "Installing %s package(s): [%s]"
          (show (length fullDependencyList))
          (unwords $ map (toString . (^. name)) fullDependencyList)
      mapM_ (installRelease decodedPackageFile) fullDependencyList
      liftIO $ putStrLn "Done"

runnableName :: PackageRelease -> Text
runnableName pkg =
  if (isJust $ pkg ^. simpleExecutableInstall)
  then last $ T.splitOn "/" (fromJust $ pkg ^. simpleExecutableInstall)
  else pkg ^. name

availableExternalManagers :: MonadIO m => m [PackageSpec.ExternalPackageType]
availableExternalManagers = do
  results :: [Maybe String] <- liftIO $ mapM findExecutable $ map (toString . PackageSpec.toPackageManagerBinaryName) PackageSpec.externalPackageTypes
  let results' =
        zipWith
          (\result extManager -> (const extManager) <$> result)
          results
          PackageSpec.externalPackageTypes
  return $ filterJustAndUnwrap results'

isBinaryExternallyAvailable :: MonadIO m => Text -> m Bool
isBinaryExternallyAvailable binaryName = do
  results <- liftIO $ findExecutables $ toString binaryName
  return . not . null $ filter (\bin -> not (".scarf/bin" `T.isInfixOf` bin)) (map toText results)

selectInstallPlan :: ScarfContext m => [InstallPlan] -> m (Maybe InstallPlan)
selectInstallPlan plans = do
  _avail <- availableExternalManagers
  return $ List.find (\(InstallPlan p a c) -> isNothing p || (p `elem` (map Just _avail))) plans

installReleaseApplication home releaseToInstall (PackageSpec.ReleaseApplication name target) = do
  let wrappedAliasPath = (toString $ wrappedProgram home name)
  liftIO $
    writeFile
      wrappedAliasPath
      (T.unlines
         [ "#!/bin/bash"
         , "function join_by { local d=$1; shift; echo -n \"$1\"; shift; printf \"%s\" \"${@/#/$d}\"; }"
         , toText $ printf "arg_string=$(join_by \"%s\" \"$@\")" delimeter
         , toText $
           printf
             "scarf execute %s --alias=\"%s\" --args \"$arg_string\""

           (releaseToInstall ^. uuid)
             (name)
         ])
  putTextLnM "Setting wrapper permissions"
  liftIO $ setFileMode wrappedAliasPath accessModes

applicationsForRelease ::
     PackageRelease -> Maybe InstallPlan -> [PackageSpec.ReleaseApplication]
applicationsForRelease release installPlan =
  let nubApps = \apps -> List.nubBy
        (\(PackageSpec.ReleaseApplication n1 _) (PackageSpec.ReleaseApplication n2 _) ->
           n1 == n2)
        (apps)
  in
    case (release ^. packageType, installPlan) of
      (NodePackage, _) ->
        PackageSpec.unReleaseApplicationObject $
        PackageSpec.getBinsFromRawNpmJson $ release ^. nodePackageJson
      (ArchivePackage, Just (InstallPlan _ (PackageSpec.ReleaseApplicationObject apps) _)) ->
        let explicitApps = nubApps apps in
          if not $ null explicitApps then explicitApps
          else [PackageSpec.ReleaseApplication (release ^. name) Nothing]
      (ExternalPackage, Just (InstallPlan _ (PackageSpec.ReleaseApplicationObject apps) _)) ->
        let explicitApps = nubApps apps in
          if not $ null explicitApps then explicitApps
          else [PackageSpec.ReleaseApplication (release ^. name) Nothing]
      (ExternalPackage, Nothing) ->
        error
          "External package types without install plans are unsupported in this version of Scarf. Try running `scarf upgrade` and try again, or this could be an issue with the package you're installing"

externalManagerInstallProcAndArgs ::
     PackageSpec.ExternalPackageType
  -> Text
  -> Maybe Text
  -> Bool
  -> (String, [String])
externalManagerInstallProcAndArgs pkgType pkgName maybeInstallCommand shouldSudo =
  let externalManagerBin = toString $ PackageSpec.toPackageManagerBinaryName pkgType
      procCall =
        if shouldSudo
          then ["sudo", externalManagerBin]
          else [externalManagerBin]
      args =
        words
          (fromMaybe
             (printf "install %s" (pkgName))
             (toString <$> maybeInstallCommand))
      allTokens = procCall ++ args
   in (head allTokens, tail allTokens)

externalManagerUninstallProcAndArgs ::
     PackageSpec.ExternalPackageType
  -> Text
  -> Bool
  -> (String, [String])
externalManagerUninstallProcAndArgs pkgType pkgName shouldSudo =
  let externalManagerBin = toString $ PackageSpec.toPackageManagerBinaryName pkgType
      procCall =
        if shouldSudo
          then ["sudo", externalManagerBin]
          else [externalManagerBin]
      uninstallArgString = case pkgType of
        PackageSpec.Debian   -> "remove"
        PackageSpec.Homebrew -> "uninstall"
        PackageSpec.NPM      -> "uninstall -g"
        PackageSpec.CPAN     -> "--uninstall"
        PackageSpec.RPM      -> "remove"
      args =
        words $ uninstallArgString ++ " " ++ (toString pkgName)
      allTokens = procCall ++ args
   in (head allTokens, tail allTokens)

getInstallPlan :: ScarfContext m => PackageRelease -> m InstallPlan
getInstallPlan release = do
    maybeInstallPlan <-
      selectInstallPlan (listInstallPlans release)
    maybeInstallPlan `orThrow`
      (PackageSpecError $ "No install plan found for package " <> (release ^. name))

-- If we get a legacy item with no install plans
listInstallPlans :: PackageRelease -> [InstallPlan]
listInstallPlans release
  | not $ null (release ^. installPlans) = release ^. installPlans
  | ((isJust $ release ^. simpleExecutableInstall) &&
     null (release ^. installPlans)) =
    let target = fromJust $ release ^. simpleExecutableInstall
        name = (last $ T.splitOn "/" target)
     in [ InstallPlan
            Nothing
            (PackageSpec.ReleaseApplicationObject
               [PackageSpec.ReleaseApplication name (Just target)])
            Nothing
        ]
  | (release ^. packageType == NodePackage) =
    [ InstallPlan
        Nothing
        (PackageSpec.getBinsFromRawNpmJson (release ^. nodePackageJson))
        Nothing
    ]
  | otherwise = release ^. installPlans

installRelease :: ScarfContext m => UserState -> PackageRelease -> m ()
installRelease decodedPackageFile releaseToInstall =
  catch
    (if isReleaseInstalled decodedPackageFile releaseToInstall
       then liftIO . putStrLn $
            printf "%s already installed" (releaseToInstall ^. name)
       else do
         home <- asks homeDirectory
         sudo <- asks useSudo
         let installDiretory =
               originalProgram home (releaseToInstall ^. uuid) <> "/"
         ifDebug $ pPrint releaseToInstall
         maybeInstallPlan <-
           selectInstallPlan (listInstallPlans releaseToInstall)
         plan@(InstallPlan maybeExternalPackageType _ maybeInstallCommand) <-
           maybeInstallPlan `orThrow`
           (PackageSpecError "No install plan found for package")
         let pkgName = releaseToInstall ^. name
         let binList = applicationsForRelease releaseToInstall maybeInstallPlan
         if isJust $ maybeExternalPackageType
        -- External Package
           then do
             let pkgType = fromJust maybeExternalPackageType
             liftIO . putStrLn $ (show pkgType) ++ " package"
          -- check if it's already installed elsewhere and skip if so
             externallyAvailable <-
               mapM
                 (isBinaryExternallyAvailable .
                  PackageSpec.releaseApplicationName)
                 binList
             _ <-
               when
                 (all id externallyAvailable)
                 (putTextLnM (pkgName <> " already installed externally") >>
                  throwM NothingToDo)
             let (processEntry, args) =
                   externalManagerInstallProcAndArgs
                     pkgType
                     (releaseToInstall ^. name)
                     (maybeInstallCommand)
                     sudo
             exitCode <- liftIO . runProcess $ proc processEntry args
             liftIO $ print exitCode
             case exitCode of
               ExitSuccess   -> return ()
               ExitFailure e -> throwM $ ExternalInstallFailed e
             _ <-
               mapM_ (installReleaseApplication home releaseToInstall) binList
             return ()
           else do
             fetchUrl <-
               (releaseToInstall ^. executableUrl) `orThrow`
               (PackageSpecError
                  "Miconfigured package: no url found. Please notify the package author")
             downloadAndInstallOriginal
               home
               releaseToInstall
               fetchUrl
               (releaseToInstall ^. includes)
             mapM_ (installReleaseApplication home releaseToInstall) binList
         logPackageInstall (releaseToInstall ^. uuid)
         let newInstalls =
               (installationsForReleaseApplications
                  releaseToInstall
                  binList
                  plan)
             updatedPackageFile =
               UserState
                 (Just $
                  newInstalls ++
                  List.deleteBy
                    (\a b -> (a ^. name) == (b ^. name))
      -- XXX - this is a bit unsafe
                    (head $ newInstalls)
                    (getDependencies decodedPackageFile))
                 (decodedPackageFile ^. packageAccess)
         let postInstallScript =
               List.find
                 (\PackageSpec.PackageScript {scriptType} ->
                    scriptType == PackageSpec.PostInstall)
                 (releaseToInstall ^. scripts)
         exitCode <-
           maybe
             (return ExitSuccess)
             (\PackageSpec.PackageScript {script} -> do
                liftIO $ putStrLn "Running post install script"
                let scriptBody =
                      "cd " ++
                      (toString installDiretory) ++ " && " ++ (toString script)
                ifDebug . liftIO $ putStrLn scriptBody
                liftIO $ hSetBuffering stdout NoBuffering
                liftIO $ hSetBuffering stderr NoBuffering
                process <- startProcess (shell scriptBody)
                exitCode <- waitExitCode process
                return exitCode)
             postInstallScript
         case exitCode of
           ExitSuccess   -> return ()
           ExitFailure e -> throwM $ PackageScriptFailed e
         writePackageFile updatedPackageFile)
    (nothingToDoHandler)

installationsForReleaseApplications ::
     PackageRelease
  -> [PackageSpec.ReleaseApplication]
  -> InstallPlan
  -> [UserInstallation]
installationsForReleaseApplications rls aliasList plan =
  map
    (\(PackageSpec.ReleaseApplication aliasName aliasTarget) ->
       UserInstallation
         (rls ^. name)
         (Just $ aliasName)
         (Just $ rls ^. uuid)
         (Just . toText . prettyShow $ rls ^. version)
         (Just $ rls ^. packageType)
         (plan ^. externalPackageType)
         (aliasTarget)
         (if ((rls ^. packageType) == NodePackage)
            then (Just "node")
            else Nothing))
    aliasList

writePackageFile :: ScarfContext m => UserState -> m ()
writePackageFile pkgFile = do
  home <- asks homeDirectory
  let userPackageFile = toString $ sysPackageFilePath home
  liftIO $
    L8.writeFile
      userPackageFile
          (AesonPretty.encodePretty pkgFile)

logPackageInstall ::
     (MonadReader Config m, MonadIO m, MonadThrow m) => Text -> m ()
logPackageInstall pkgUuid = do
  base <- asks backendBaseUrl
  request <-
    buildRequestWithTokenAuth
      (toText base <> "/package-event/install/" <> pkgUuid)
      "POST"
  response <- httpBS request
  if getResponseStatusCode response == 200
    then return ()
    else throwM $
         UnknownError
           ("[" <> (toText . show $ getResponseStatus response) <> "] Message: " <>
            (toText . show $ response))

latestRelease ::
  ScarfContext m
  => PackageSpec.Platform
  -> PackageDetails
  -> VersionRange
  -> m (Maybe PackageRelease)
latestRelease releasePlatform details versionRange = do
  filteredResults <-
    filterM
      (\r -> do
         plan <- selectInstallPlan (listInstallPlans r)
         return $
           (isJust plan) &&
           (withinRange (r ^. version) versionRange) &&
           (r ^. platform == releasePlatform ||
            r ^. platform == PackageSpec.AllPlatforms))
      (details ^. releases)
  let maybeLatestVersion =
        safeHead . List.sortOn Data.Ord.Down $
        map (^. version) $ filteredResults
  return $
    maybeLatestVersion >>=
    (\latestVersion ->
       List.find (\r -> r ^. version == latestVersion) (filteredResults))

downloadArchive :: (MonadIO m) => Text -> Text -> m ()
downloadArchive url saveAsPath = do
  request <- liftIO $ parseRequest $ T.unpack url
  downloadResp <- httpBS request
  liftIO $ L8.writeFile (toString saveAsPath) (L8.fromStrict $ getResponseBody downloadResp)

-- TODO(#error-handling) catch installation IO exceptions for nicer errors
downloadAndInstallOriginal ::
     (ScarfContext m) => FilePath -> PackageRelease -> Text -> [FilePath] -> m ()
downloadAndInstallOriginal homeDir release url toInclude =
  let extension
        | ".zip" `T.isSuffixOf` url = ".zip"
        | ".tar.gz" `T.isSuffixOf` url || ".tgz" `T.isSuffixOf` url = ".tar.gz"
        | otherwise = fail $ "Unsupported archive format: " ++ (toString url)
      tmpArchive = "/tmp/tmp-scarf-package-install" <> extension
      tmpArchiveExtracedFolder = "/tmp/tmp-scarf-package-install"
      archiveExtractFunction =
        if extension == ".tar.gz"
          -- tarball
          then (\archivePath ->
                  (Tar.unpack tmpArchiveExtracedFolder .
                   Tar.read . GZ.decompress) =<<
                  L8.readFile archivePath)
          -- zip
          else (\archivePath ->
                  withArchive archivePath (unpackInto tmpArchiveExtracedFolder))
   in do (InstallPlan _ (PackageSpec.ReleaseApplicationObject apps) _) <-
           getInstallPlan release
         let extractedBins =
               map
                 (\(PackageSpec.ReleaseApplication name target) ->
                    tmpArchiveExtracedFolder ++
                    "/" ++ (toString $ fromMaybe name target))
                 (apps)
             installDiretory = originalProgram homeDir (release ^. uuid) <> "/"
             installPath = installDiretory <> (release ^. uuid)
         liftIO $ removePathForcibly tmpArchiveExtracedFolder
         liftIO $ createDirectoryIfMissing True (toString installDiretory)
         liftIO $ putStrLn "Downloading"
         request <- liftIO $ parseRequest $ T.unpack url
         downloadResp <- httpBS request
         let responseBody = L8.fromStrict $ getResponseBody downloadResp
         _ <-
           liftIO $
           L8.writeFile
             tmpArchive
             responseBody
         let receivedHash = toText . show $ hashWith SHA256 (L8.toStrict responseBody)
         mapM (\sig -> when (sig /= receivedHash) (throwM (InvalidSignature sig receivedHash))) (release ^. executableSignature)
         liftIO $ putStrLn "Extracting..."
         liftIO $ archiveExtractFunction tmpArchive
         liftIO $ putStrLn "Copying..."
         liftIO $
           when
             (release ^. packageType == ArchivePackage)
             (mapM_
                (\bin -> do
                   permissions <- liftIO $ getPermissions bin
                   setPermissions bin (setOwnerExecutable True permissions)
                   copyFile bin $ toString installPath)
                extractedBins)
         liftIO $
           forM_
             toInclude
             (\pathToCopy -> do
                res <-
                  copyFileOrDir
                    (tmpArchiveExtracedFolder <> "/" <> toString pathToCopy)
                    (toString installDiretory)
                case res of
                  ExitSuccess -> return ()
                  ExitFailure i ->
                    putStrLn $
                    "Error copying " ++ toString pathToCopy ++ ": " ++ show i)
         liftIO $
           when (isJust (release ^. nodePackageJson)) $
           (runProcess $
            proc "npm" $
            words
              (printf "--prefix %s install %s" installDiretory installDiretory)) >>
           return ()

semVersionSort :: [Text] -> [Text]
semVersionSort [] = []
semVersionSort x  = List.sortBy semVerComp x

semVerComp :: Text -> Text -> Ordering
semVerComp a b = compPerPart (T.splitOn "." a) (T.splitOn "." b)

compPerPart :: [Text] -> [Text] -> Ordering
compPerPart [] [] = EQ
compPerPart _ [] = LT
compPerPart [] _ = GT
compPerPart (x:xs) (y:ys)
  | x == y = compPerPart xs ys
  | T.isInfixOf "-" x || T.isInfixOf "-" y = compPerPart (T.splitOn "-" x) (T.splitOn "-" y)
  | otherwise = compare x y

makeCliError :: ServantError -> CliError
makeCliError (FailureResponse (ServantClientCore.Response (Status 404 s) h v d)) = NotFoundError (decodeUtf8 s)
makeCliError s = CliConnectionError . toText $ show s

lintPackageFile :: (ScarfContext m) => FilePath -> m ValidatedPackageSpec
lintPackageFile f
  | ".yaml" `T.isSuffixOf` f = lintYamlPackageFile f
  | ".json" `T.isSuffixOf` f = lintNpmPackageFile f
  | otherwise = throwM $ UserError "Scarf package files must be .yaml or .json"

adjustPath :: (ScarfContext m) => FilePath -> m FilePath
adjustPath f =
  asks homeDirectory >>= \home ->
    return $ T.replace "~" home f

getUnvalidatedPackageFile :: (ScarfContext m) => FilePath -> m PackageSpec.PackageSpec
getUnvalidatedPackageFile f
  | ".json" `T.isSuffixOf` f = getUnvalidatedNpmPackageFile f
  | ".yaml" `T.isSuffixOf` f = getUnvalidatedYamlPackageFile f
  | otherwise = throwM $ UserError "Scarf package files must be .yaml or .json"

getUnvalidatedNpmPackageFile :: (ScarfContext m) => FilePath -> m PackageSpec.PackageSpec
getUnvalidatedNpmPackageFile f = do
  adjustedF <- adjustPath f
  rawPackageJson <- liftIO $ TIO.readFile (toString adjustedF)
  let (value :: Maybe Value) = decode (L8.fromStrict $ encodeUtf8 rawPackageJson)
  (decoded :: Either String PackageSpec.PackageSpec) <-
    liftIO $ eitherDecodeFileStrict (toString adjustedF)
  either
    (throwM . PackageSpecError . toText)
    (\p ->
       return $
       p
         { PackageSpec.distributions =
             ([ PackageSpec.NodeDistribution
                  rawPackageJson
                  (PackageSpec.Dependencies [])
                  (PackageSpec.getBinsFromRawNpmJson (Just rawPackageJson))
              ])
         })
    decoded

getUnvalidatedYamlPackageFile :: (ScarfContext m) => FilePath -> m PackageSpec.PackageSpec
getUnvalidatedYamlPackageFile f = do
  adjustedF <- adjustPath f
  (decoded :: Either Yaml.ParseException PackageSpec.PackageSpec) <-
    liftIO $ Yaml.decodeFileEither (toString adjustedF)
  either
    (throwM . PackageSpecError . toText . show)
    (return)
    decoded

lintNpmPackageFile :: (ScarfContext m) => FilePath -> m ValidatedPackageSpec
lintNpmPackageFile f = do
  adjustedF <- adjustPath f
  rawPackageJson <- liftIO $ TIO.readFile (toString adjustedF)
  (decoded :: Either String PackageSpec.PackageSpec) <- liftIO $ eitherDecodeFileStrict (toString adjustedF)
  validated <- either (throwM . PackageSpecError . toText) (\p -> return $ validateSpec p (Just rawPackageJson)) decoded
  let distributionList = fillDistrubtionsNpm rawPackageJson
  either (throwM . PackageSpecError) (\s -> pPrint s >> return s) validated

lintYamlPackageFile  :: (ScarfContext m) => FilePath -> m ValidatedPackageSpec
lintYamlPackageFile f = do
  unvalidated <- getUnvalidatedYamlPackageFile f
  let validated = validateSpec unvalidated Nothing
  either (throwM . PackageSpecError) (\v -> (liftIO $ pPrint v) >> return v) validated

textUUID :: MonadIO m => m Text
textUUID = liftIO $ UUID.toText <$> UUID4.nextRandom

intersection :: Eq a => [a] -> [a] -> [a]
intersection (x:xs) ys = if x `elem` ys
                 then x:intersection xs (List.delete x ys)
                 else intersection xs ys
intersection [] _ = []

hostPlatform :: PackageSpec.Platform
hostPlatform =
  case (os, arch) of
    ("darwin", _)       -> PackageSpec.MacOS
    ("linux", "x86_64") -> PackageSpec.Linux_x86_64
    pair                -> error $ "Unsupported platform: " <> show pair

fillDistrubtionsNpm :: Text -> [PackageSpec.PackageDistribution]
fillDistrubtionsNpm rawPackageJson =
  [ PackageSpec.NodeDistribution
      rawPackageJson
      (PackageSpec.Dependencies [])
      (PackageSpec.getBinsFromRawNpmJson (Just rawPackageJson ))
  ]

validateSpec :: PackageSpec.PackageSpec -> Maybe Text -> Either Text ValidatedPackageSpec
validateSpec (PackageSpec.PackageSpec n v a c l d) Nothing = do
  license <-
    maybe (Left $ "Couldn't parse license " <> l) (Right) $
    simpleParse (toString l)
  version <- mapLeft toText $ eitherParsec (toString v)
  if null d
    then Left "No distributions found"
    else Right $ ValidatedPackageSpec n version a c license d
validateSpec (PackageSpec.PackageSpec n v a c l _) (Just rawJson) = do
  license <-
    maybe (Left $ "Couldn't parse license " <> l) (Right) $
    simpleParse (toString l)
  version <- mapLeft toText $ eitherParsec (toString v)
  Right $
    ValidatedPackageSpec n version a c license (fillDistrubtionsNpm rawJson)

sendFeedback :: ScarfContext m => m ()
sendFeedback = do
  liftIO $ TIO.putStr "Name: " >> hFlush stdout
  name <- liftIO TIO.getLine
  liftIO $ TIO.putStr "Email: " >> hFlush stdout
  email <- liftIO TIO.getLine
  liftIO $ TIO.putStr "Feedback: " >> hFlush stdout
  feedback <- liftIO TIO.getLine
  manager' <- asks httpManager
  base <- asks backendBaseUrl
  parsedBaseUrl <- parseBaseUrl base
  _ <-
    liftIO $
    runClientM
      (askSendFeedback $ FeedbackRequest email name feedback)
      (mkClientEnv manager' parsedBaseUrl)
  liftIO $ putStrLn "Thanks for your feedback!"
  return ()
