{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
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
import qualified Control.Exception                     as Exception
import           Control.Exception.Safe                (Exception, MonadThrow,
                                                        SomeException, catch,
                                                        catchAsync, catchIO,
                                                        throwM)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty              as AesonPretty
import qualified Data.ByteString.Lazy.Char8            as L8
import           Data.Either
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
import qualified Dhall
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
import           System.Posix.Files
import           System.Process.Typed
import           Text.Pretty.Simple
import           Text.Printf
import           Text.Read

exitNum :: ExitCode -> Integer
exitNum ExitSuccess     = 0
exitNum (ExitFailure i) = fromIntegral i

type ScarfContext m = (MonadReader Config m, MonadIO m, MonadThrow m)
type IOConfigContext m = (MonadReader Config m, MonadIO m)

runProgramWrapped :: (IOConfigContext m) => FilePath -> Text -> m ExecutionResult
runProgramWrapped f argString =
  let argsToPass = filter (/= "") (T.splitOn delimeter argString)
      safeArgString = redactArguments argsToPass
      uuid = head $ T.splitOn delimeter f
  in do home <- asks homeDirectory
        maybeToken <- asks userApiToken
        base <- asks backendBaseUrl
        start <- liftIO $ (round . (* 1000)) `fmap` getPOSIXTime
        exitCode <-
          liftIO $
          Exception.catch
            (runProcess $
             proc
               (toString (originalProgram home f <> "/" <> f))
               (map toString argsToPass))
            (\(err :: Exception.SomeException) -> do
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
      -- TODO - logging
        _ <- httpBS request
        return $ ExecutionResult exitCode (fromIntegral runtime) safeArgString

redactArguments :: [Text] -> [Text]
redactArguments argList =
  foldl
    (\tokens currentArg ->
       if ("-" `T.isPrefixOf` (fromMaybe "" $ safeLast tokens)) && (not $ "-" `T.isPrefixOf` currentArg)
         then tokens ++ ["<REDACTED_ARG>"]
         else tokens ++ [currentArg])
    []
    argList

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast x  = Just $ last x

-- FIXME the type of this function is bad
buildRequestWithTokenAuth :: (MonadReader Config m, MonadIO m) => Text -> Text -> m Request
buildRequestWithTokenAuth url httpMethod = do
  maybeToken <- asks userApiToken
  initReq <- liftIO . parseRequest $ toString url
  return $ (if isJust maybeToken
      then setRequestBasicAuth "n/a" (encodeUtf8 $ fromJust maybeToken)
      else Prelude.id) (initReq {method = encodeUtf8 httpMethod})


uploadPackageRelease :: (MonadReader Config m, MonadIO m, MonadThrow m) => FilePath -> m ()
uploadPackageRelease f = do
  home <- asks homeDirectory
  (token :: Text) <-
    maybe (throwM NoCredentialsError) return =<< asks userApiToken
  httpMgr <- asks httpManager
  base <- asks backendBaseUrl
  let adjustedF = T.replace "~" home f
  dhallRaw <- liftIO . TIO.readFile $ T.unpack adjustedF
  (parsedSpec :: PackageSpec.PackageSpec) <- parseDhallOrThrow adjustedF
  spec <-
    either
      (\err -> throwM . PackageSpecError $ "Error validating your spec: " <> err)
      return
      (validateSpec parsedSpec)
  liftIO $ putStrLn "Uploading release"
  pPrint spec
  -- upload any archives
  let distrubutionsToUpload =
        filter (not . isRemoteUrl . PackageSpec.uri) $
        spec ^. distributions
      archiveFileParts =
        map
          (\dist ->
             partFileSource
               ("archive-" <> (toText . show $ PackageSpec.platform dist))
               (toString $ PackageSpec.uri dist))
          distrubutionsToUpload
  initReq <- liftIO $ parseRequest $ base ++ "/package/release"
  let request =
        setRequestBasicAuth "n/a" (encodeUtf8 token) $
        initReq {method = "POST"}
  formified <-
    formDataBody
       ([ partFileRequestBody "spec" "spec.json" $
          RequestBodyBS (L8.toStrict $ encode parsedSpec)
        ] ++
        archiveFileParts)
       request
  response <- liftIO $ Network.HTTP.Client.httpLbs formified httpMgr
  if getResponseStatusCode response == 200
    then void $ pPrint "Upload complete!"
    else void $ pPrint
           ("[" <> (show $ getResponseStatus response) <> "] Message: " <>
            (show $ response))

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
  mapM_ (\(i :: UserInstallation) -> installProgramWrapped (i ^. name)) (getDependencies userState)

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

installProgramWrapped ::
     (MonadReader Config m, MonadIO m, MonadThrow m) => Text -> m ()
installProgramWrapped pkgName = do
  home <- asks homeDirectory
  base <- asks backendBaseUrl
  _ <- setUpScarfDirs
  manager' <- asks httpManager
  parsedBaseUrl <- parseBaseUrl base
  liftIO . putStrLn . toString $ "Installing " <> pkgName
  _details <-
    liftIO $
      runClientM
         (askGetPackageDetails pkgName)
         (mkClientEnv manager' parsedBaseUrl)
  let userPackageFile = toString $ home <> "/.scarf/scarf-package.json"
  case _details of
    Left servantErr -> throwM $ makeCliError servantErr
    Right details -> do
      let maybeRelease = latestRelease hostPlatform details
      when (isNothing maybeRelease) $ throwM $ NotFoundError "No release found"
      let releaseToInstall = fromJust maybeRelease
      let fetchUrl = releaseToInstall ^. executableUrl
      let wrappedProgramPath = toString $ wrappedProgram home pkgName
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
      liftIO $ printf "Installation complete: %s\n" wrappedProgramPath
      liftIO $ putStrLn "Writing state to package file"
      decodedPackageFile <- readSysPackageFile
      let newInstall =
            UserInstallation
              pkgName
              (Just $ releaseToInstall ^. uuid)
              (Just $ releaseToInstall ^. version)
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
      liftIO $ putStrLn "Done"

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
     PackageSpec.Platform -> PackageDetails -> Maybe PackageRelease
latestRelease releasePlatform details =
  let maybeLatestVersion =
        safeHead . List.sortOn Data.Ord.Down $
        map (^. version) $
        filter (\r -> r ^. platform == releasePlatform) (details ^. releases)
  in maybeLatestVersion >>=
     (\latestVersion ->
        List.find
          (\r ->
             r ^. platform == releasePlatform && r ^. version == latestVersion)
          (details ^. releases))

-- TODO(#error-handling) catch installation IO exceptions for nicer errors
downloadAndInstallOriginal ::
     (MonadIO m) => FilePath -> PackageRelease -> Text -> [FilePath] -> m ()
downloadAndInstallOriginal homeDir release url toInclude =
  let tmpArchive = "/tmp/tmp-u-package-install.tar.gz"
      tmpArchiveExtracedFolder = "/tmp/tmp-scarf-package-install"
      tmpExtractedBin =
        tmpArchiveExtracedFolder <> "/" <>
        (toString . fromJust $ release ^. simpleExecutableInstall)
      executableName = fromJust $ release ^. simpleExecutableInstall
      installDiretory = originalProgram homeDir (release ^. uuid) <> "/"
      installPath = installDiretory <> (release ^. uuid)
  in liftIO $ do
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
       permissions <- liftIO $ getPermissions tmpExtractedBin
       setPermissions tmpExtractedBin (setOwnerExecutable True permissions)
       copyFile tmpExtractedBin $ toString installPath
       forM_
         toInclude
         (\pathToCopy -> do
            res <-
              copyFileOrDir (tmpArchiveExtracedFolder <> "/" <> toString pathToCopy) (toString installDiretory)
            case res of
              ExitSuccess -> return ()
              ExitFailure i ->
                putStrLn $
                "Error copying " ++ toString pathToCopy ++ ": " ++ show i)

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

lintDhallPackageFile :: (MonadReader Config m, MonadIO m) => FilePath -> m (Either Text ValidatedPackageSpec)
lintDhallPackageFile f = do
  home <- asks homeDirectory
  let pathFixed = T.replace "~" home f
  (parsedPackage :: Either Text PackageSpec.PackageSpec) <- parseDhallEither pathFixed
  case parsedPackage of
    Left err -> do
      liftIO . putStrLn $ toString err
      return $ Left err
    result@(Right pkg) ->
      let validated = validateSpec pkg in
        either (liftIO . putStrLn . toString) pPrint validated >>
          return validated

parseDhallOrThrow :: (MonadIO m, Dhall.Interpret a, MonadThrow m) => FilePath -> m a
parseDhallOrThrow f =
  parseDhallEither f >>= \result ->
    either throwM return result

parseDhallEither :: (MonadIO m, Dhall.Interpret a) => FilePath -> m (Either Text a)
parseDhallEither f =
  liftIO $ Exception.catch
    (Right <$> Dhall.input Dhall.auto f)
    (\(err :: Exception.SomeException) -> return . Left . toText $ show err)

textUUID :: MonadIO m => m Text
textUUID = liftIO $ UUID.toText <$> UUID4.nextRandom


intersection :: Eq a => [a] -> [a] -> [a]
intersection (x:xs) ys = if x `elem` ys
                 then x:intersection xs (List.delete x ys)
                 else intersection xs ys
intersection [] _ = []

filterJustAndUnwrap = Data.Maybe.catMaybes

hostPlatform :: PackageSpec.Platform
hostPlatform =
  case (os, arch) of
    ("darwin", _)       -> PackageSpec.MacOS
    ("linux", "x86_64") -> PackageSpec.Linux_x86_64
    pair                -> error $ "Unsupported platform: " <> show pair

validateSpec :: PackageSpec.PackageSpec -> Either Text ValidatedPackageSpec
validateSpec (PackageSpec.PackageSpec n v a c l d) =
  let result = readEither (toString l) >>=
        (\validLic -> Right $ ValidatedPackageSpec n v a c validLic d)
  in mapLeft (const $ "Couldn't parse license type: \"" <> l <> "\"") result
