{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Codec.Compression.GZip                as GZ
import qualified Control.Exception                     as UnsafeException
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty              as AesonPretty
import qualified Data.ByteString                       as BS
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
import           Distribution.Compat.Graph             (Key)
import qualified Distribution.Compat.Graph             as Graph
import           Distribution.Parsec.Class
import           Distribution.Pretty
import           Distribution.Types.VersionRange
import           Distribution.Version
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
import           System.Log.Logger
import           System.Posix.Files
import           System.Process.Typed
import           Text.Pretty.Simple
import           Text.Printf
import           Text.Read


scarfCliVersion :: Text
scarfCliVersion = "0.5.0"

exitNum :: ExitCode -> Integer
exitNum ExitSuccess     = 0
exitNum (ExitFailure i) = fromIntegral i

type ScarfContext m = (MonadReader Config m, MonadIO m, MonadThrow m)
type IOConfigContext m = (MonadReader Config m, MonadIO m)

runProgramWrapped :: (ScarfContext m) => FilePath -> Text -> m ExecutionResult
runProgramWrapped f argString =
  let argsToPass =
        filter (/= "") $ splitArgTokens (T.splitOn delimeter argString)
      safeArgString = redactArguments argsToPass
      uuid = head $ T.splitOn delimeter f
  in do home <- asks homeDirectory
        maybeToken <- asks userApiToken
        base <- asks backendBaseUrl
        sysPkgFile <- readSysPackageFile
        pkgEntry <- getPackageEntryForUuid sysPkgFile f
        let pkgType = pkgEntry ^. packageType
        let nodeEntryPoint = pkgEntry ^. entryPoint
        let invocation =
              case pkgType of
                NodePackage -> (toString ("node"))
                ArchivePackage ->
                  (toString (originalProgram home f <> "/" <> f))
            args =
              case pkgType of
                NodePackage ->
                  map toString $
                  (originalProgram home f <> "/" <> fromJust nodeEntryPoint) :
                  argsToPass
                ArchivePackage -> (map toString argsToPass)
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
        initReq <- liftIO $ parseRequest $ base ++ "/package-call"
        let request =
              (if isJust maybeToken
                 then setRequestBasicAuth
                        "n/a"
                        (encodeUtf8 $ fromJust maybeToken)
                 else Prelude.id) $
              setRequestBodyJSON packageCallToLog $ initReq {method = "POST"}
        _ <-
          liftIO $
          UnsafeException.catch
            (void $ httpBS request)
            (\(err :: UnsafeException.SomeException) ->
               void $
               warningM "Scarf" $ "Couldn't log package call: " <> show err)
        return $ ExecutionResult exitCode (fromIntegral runtime) safeArgString

getPackageTypeForUuid :: (ScarfContext m) => UserState -> Text -> m PackageType
getPackageTypeForUuid (UserState Nothing) uuid =
  throwM $
  UserStateCorrupt $
  "Couldn't find installation entry in package file. Please try reinstalling. Package: " <>
  uuid
getPackageTypeForUuid (UserState (Just installs)) thisUuid =
  let entry = List.find (\i -> (fromMaybe "baduuid" $ i ^. uuid) == thisUuid) installs in
    maybe (getPackageTypeForUuid (UserState Nothing) thisUuid) (\e -> return $ e ^. packageType) entry


getPackageEntryForUuid :: (ScarfContext m) => UserState -> Text -> m UserInstallation
getPackageEntryForUuid (UserState Nothing) uuid =
  throwM $
  UserStateCorrupt $
  "Couldn't find installation entry in package file. Please try reinstalling. Package: " <>
  uuid
getPackageEntryForUuid (UserState (Just installs)) thisUuid =
  let entry = List.find (\i -> (fromMaybe "baduuid" $ i ^. uuid) == thisUuid) installs in
    maybe (getPackageEntryForUuid (UserState Nothing) thisUuid) (return) entry

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

redactArguments :: [Text] -> [Text]
redactArguments argList =
  foldl
    (\tokens currentArg ->
       if ("-" `T.isPrefixOf` (fromMaybe "" $ safeLast tokens)) && (not $ "-" `T.isPrefixOf` currentArg)
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
             {readable = True, executable = True, searchable = True, writable = True})
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
  when (isJust $ List.find (PackageSpec.isNodeDistribution) $ validatedSpec ^. distributions) (do
      let filteredItems =
            [ i
            | i <- thisDirectory
            , i `notElem` ["node_modules", ".git", ".gitignore"]
            ]
      liftIO $
        L8.writeFile "/tmp/scarf-node-archive.tar.gz" . GZ.compress . Tar.write =<<
        Tar.pack "." filteredItems)
  -- upload any archives
  let distrubutionsToUpload =
        filter
          (\d ->
             (PackageSpec.isNodeDistribution d) ||
             (not . isRemoteUrl $ PackageSpec.archiveDistributionUri d)) $
        validatedSpec ^. distributions
      archiveFileParts =
        map
          (\dist ->
             case dist of
               PackageSpec.ArchiveDistribution {..} ->
                 partFileSource
                   ("archive-" <> (toText . show $ PackageSpec.archiveDistributionPlatform dist))
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
         pPrint
           ("[" <> (show $ getResponseStatus response) <> "] Message: " <>
            (show response))

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
      else return . Right $ UserState Nothing
  when
    (isLeft decodedPackageFile)
    (throwM . UserStateCorrupt . toText $ show decodedPackageFile)
  return $ fromRight (UserState Nothing) decodedPackageFile

getDepInstallList :: PackageRelease -> [PackageRelease] -> [PackageRelease]
getDepInstallList release allReleases =
  let d@(PackageSpec.Dependencies deps) = (release ^. depends)
  in List.nubBy (\r1 r2 -> r1 ^. name == r2 ^. name) $ [release] ++
     concatMap
       (\dep ->
          (flip getDepInstallList allReleases) . fromJust $
          getLatestReleasesForDependency dep allReleases)
       deps

getLatestReleaseByName :: [PackageRelease] -> Text -> Maybe PackageRelease
getLatestReleaseByName allPkgs query =
  safeLast $
  List.sortOn (^. version) $ filter (\r -> r ^. name == query) allPkgs

getLatestReleasesForDependency ::
     PackageSpec.Dependency -> [PackageRelease] -> Maybe PackageRelease
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

isReleaseInstalled :: UserState -> PackageRelease -> Bool
isReleaseInstalled state rls =
  let deps = getDependencies state
  in isJust $ List.find
       (\dep ->
          ((dep ^. name) == (rls ^. name)) &&
          withinRange
            (rls ^. version)
            (fromMaybe anyVersion $ join $ (eitherToMaybe . eitherParsec . toString) <$> (dep ^. version)))
       deps

installProgramWrapped ::
     (ScarfContext m) => Text -> Maybe Text -> m ()
installProgramWrapped pkgName maybeVersion = do
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
      let maybeRelease =
            latestRelease
              hostPlatform
              details
              (fromMaybe anyVersion validatedVersion)
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
      let fullDependencyList = getDepInstallList releaseToInstall index
      liftIO . putStrLn $
        printf
          "Installing %s package(s): [%s]"
          (show (length fullDependencyList))
          (unwords $ map (toString . (^. name)) fullDependencyList)
      mapM_ (installRelease decodedPackageFile) fullDependencyList
      liftIO $ putStrLn "Done"

installRelease :: ScarfContext m => UserState -> PackageRelease -> m ()
installRelease decodedPackageFile releaseToInstall =
  if isReleaseInstalled decodedPackageFile releaseToInstall
    then liftIO . putStrLn $ printf "%s already installed" (releaseToInstall ^. name)
    else do
      home <- asks homeDirectory
      let userPackageFile = toString $ home <> "/.scarf/scarf-package.json"
      let fetchUrl = releaseToInstall ^. executableUrl
      let pkgName = releaseToInstall ^. name
      let wrappedProgramPath = toString $ wrappedProgram home pkgName
      let nodeEntryPoint =
            if (releaseToInstall ^. packageType == NodePackage)
              then (\(v :: Object) ->
                      let (Data.Aeson.String entry) = v HM.! "main"
                      in entry) <$>
                  (releaseToInstall ^. nodePackageJson >>=
                    (decode . L8.fromStrict . encodeUtf8))
              else Nothing
      downloadAndInstallOriginal
        home
        releaseToInstall
        fetchUrl
        (releaseToInstall ^. includes)
      liftIO $
        writeFile
          wrappedProgramPath
          (T.unlines
            [ "#!/bin/bash"
            , "function join_by { local d=$1; shift; echo -n \"$1\"; shift; printf \"%s\" \"${@/#/$d}\"; }"
            , toText $ printf "arg_string=$(join_by \"%s\" \"$@\")" delimeter
            , toText $
              printf
                "scarf execute %s --args \"$arg_string\""
                (releaseToInstall ^. uuid)
            ])
      liftIO $ setFileMode wrappedProgramPath accessModes
      logPackageInstall (releaseToInstall ^. uuid)
      let newInstall =
            UserInstallation
              pkgName
              (Just $ releaseToInstall ^. uuid)
              (Just . toText . prettyShow $ releaseToInstall ^. version)
              (releaseToInstall ^. packageType)
              nodeEntryPoint
          updatedPackageFile =
            (UserState . Just) $
            newInstall :
            List.deleteBy
              (\a b -> (a ^. name) == (b ^. name))
              newInstall
              (getDependencies decodedPackageFile)
      liftIO $
        L8.writeFile
          userPackageFile
          (AesonPretty.encodePretty updatedPackageFile)

logPackageInstall :: (MonadReader Config m, MonadIO m, MonadThrow m) => Text -> m ()
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
     PackageSpec.Platform -> PackageDetails -> VersionRange -> Maybe PackageRelease
latestRelease releasePlatform details versionRange =
  let maybeLatestVersion =
        safeHead . List.sortOn Data.Ord.Down $
        map (^. version) $
        filter
          (\r ->
             (withinRange (r ^. version) versionRange) &&
             (r ^. platform == releasePlatform ||
             r ^. platform == PackageSpec.AllPlatforms))
          (details ^. releases)
  in maybeLatestVersion >>=
     (\latestVersion ->
        List.find
          (\r ->
             (r ^. platform == releasePlatform ||
              r ^. platform == PackageSpec.AllPlatforms) &&
             r ^. version == latestVersion)
          (details ^. releases))

downloadArchive :: (MonadIO m) => Text -> Text -> m ()
downloadArchive url saveAsPath = do
  request <- liftIO $ parseRequest $ T.unpack url
  downloadResp <- httpBS request
  liftIO $ L8.writeFile (toString saveAsPath) (L8.fromStrict $ getResponseBody downloadResp)

-- TODO(#error-handling) catch installation IO exceptions for nicer errors
downloadAndInstallOriginal ::
     (MonadIO m) => FilePath -> PackageRelease -> Text -> [FilePath] -> m ()
downloadAndInstallOriginal homeDir release url toInclude =
  let tmpArchive = "/tmp/tmp-u-package-install.tar.gz"
      tmpArchiveExtracedFolder = "/tmp/tmp-scarf-package-install"
      maybeTmpExtractedBin =
        fmap (\exe ->
          tmpArchiveExtracedFolder <> "/" <>
          (toString exe)) (release ^. simpleExecutableInstall)
      installDiretory = originalProgram homeDir (release ^. uuid) <> "/"
      installPath = installDiretory <> (release ^. uuid)
  in liftIO $ do
       removePathForcibly tmpArchiveExtracedFolder
       createDirectoryIfMissing True (toString installDiretory)
       putStrLn "Downloading"
       request <- parseRequest $ T.unpack url
       downloadResp <- httpBS request
       _ <-
         L8.writeFile tmpArchive (L8.fromStrict $ getResponseBody downloadResp)
       putStrLn "Extracting..."
       Tar.unpack tmpArchiveExtracedFolder . Tar.read . GZ.decompress =<<
         L8.readFile tmpArchive
       putStrLn "Copying..."
       when (release ^. packageType == ArchivePackage) (do
        let tmpExtractedBin = fromJust maybeTmpExtractedBin
        permissions <- liftIO $ getPermissions tmpExtractedBin
        setPermissions tmpExtractedBin (setOwnerExecutable True permissions)
        copyFile tmpExtractedBin $ toString installPath)
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
  | otherwise = throwM $ UserError "Scarf package files must be .dhall or .json"

adjustPath :: (ScarfContext m) => FilePath -> m FilePath
adjustPath f =
  asks homeDirectory >>= \home ->
    return $ T.replace "~" home f

getUnvalidatedPackageFile :: (ScarfContext m) => FilePath -> m PackageSpec.PackageSpec
getUnvalidatedPackageFile f
  | ".json" `T.isSuffixOf` f = getUnvalidatedNpmPackageFile f
  | ".yaml" `T.isSuffixOf` f = getUnvalidatedYamlPackageFile f
  | otherwise = throwM $ UserError "Scarf package files must be .dhall or .json"

getUnvalidatedNpmPackageFile :: (ScarfContext m) => FilePath -> m PackageSpec.PackageSpec
getUnvalidatedNpmPackageFile f = do
  adjustedF <- adjustPath f
  rawPackageJson <- liftIO $ TIO.readFile (toString adjustedF)
  (decoded :: Either String PackageSpec.PackageSpec) <-
    liftIO $ eitherDecodeFileStrict (toString adjustedF)
  either
    (throwM . PackageSpecError . toText)
    (\p ->
       return $
       p
       { PackageSpec.distributions =
           ([PackageSpec.NodeDistribution rawPackageJson (PackageSpec.Dependencies [])])
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
  [ PackageSpec.NodeDistribution rawPackageJson (PackageSpec.Dependencies [])
  ]

validateSpec :: PackageSpec.PackageSpec -> Maybe Text -> Either Text ValidatedPackageSpec
validateSpec (PackageSpec.PackageSpec n v a c l d) Nothing = do
  license <- mapLeft toText $ readEither (toString l)
  version <- mapLeft toText $ eitherParsec (toString v)
  if null d
    then Left "No distributions found"
    else mapLeft (const $ "Couldn't parse license type: \"" <> l <> "\"") $
         Right $ ValidatedPackageSpec n version a c license d
validateSpec (PackageSpec.PackageSpec n v a c l _) (Just rawJson) = do
  license <- mapLeft toText $ readEither (toString l)
  version <- mapLeft toText $ eitherParsec (toString v)
  mapLeft (const $ "Couldn't parse license type: \"" <> l <> "\"") $
    Right $
    ValidatedPackageSpec n version a c license (fillDistrubtionsNpm rawJson)
