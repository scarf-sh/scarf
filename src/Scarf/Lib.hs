{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}


module Scarf.Lib where

import           Scarf.Client
import           Scarf.Common
import qualified Scarf.PackageSpec                     as PackageSpec
import           Scarf.Types

import qualified Codec.Archive.Tar                     as Tar
import qualified Codec.Compression.GZip                as GZ
import qualified Control.Exception                     as Exception
import           Control.Exception.Safe                (Exception, MonadThrow,
                                                        SomeException, throwM)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Crypto.JOSE.JWK
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as L
import qualified Data.ByteString.Lazy.Char8            as L8
import           Data.Char
import qualified Data.List                             as List
import           Data.Maybe
import           Data.Pool
import qualified Data.SemVer                           as SemVer
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding
import           Data.Text.IO                          hiding (putStrLn)
import qualified Data.Text.IO                          as TIO
import qualified Data.Text.Lazy                        as TL
import qualified Data.Text.Lazy                        as TL
import           Data.Time.Clock.POSIX
import qualified Data.UUID                             as UUID
import qualified Data.UUID.V4                          as UUID4
import qualified Dhall                                 as Dhall
import           Distribution.License
import qualified Distribution.Types.Version
import           DynFlags
import           GHC.Generics
import           Lens.Micro.Platform
import           Network.HTTP.Client
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Client.TLS
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Prelude                               hiding (FilePath,
                                                        writeFile)
import           Servant.Auth.Server
import           Servant.Client
import           System.Directory
import           System.Exit
import           System.Info
import           System.IO                             (hClose, hPutStr)
import           System.Posix.Files
import           System.Posix.Types
import           System.Process.Typed
import           Text.Pretty.Simple
import           Text.Printf
import           Text.Read


exitNum :: ExitCode -> Integer
exitNum ExitSuccess     = 0
exitNum (ExitFailure i) = fromIntegral i

runProgramWrapped :: (MonadReader Config m, MonadIO m) => FilePath -> Text -> m ExecutionResult
runProgramWrapped f argString =
  let argsToPass = filter (/= "") (T.splitOn delimeter argString)
      safeArgString = redactArguments argsToPass
      uuid = head $ T.splitOn delimeter f
  in do home <- asks homeDirectory
        maybeToken <- asks userApiToken
        base <- asks backendBaseUrl
        start <- liftIO $ (round . (* 1000)) `fmap` getPOSIXTime
        exitCode <-
          runProcess $
          proc (toString $ originalProgram home f) (map toString argsToPass)
        end <- liftIO $ (round . (* 1000)) `fmap` getPOSIXTime
        let runtime = (end - start)
            packageCallToLog =
              CreatePackageCallRequest uuid (exitNum exitCode) runtime (T.intercalate delimeter safeArgString)
        -- TODO(#configuration) - the server should be configurable
        initReq <- liftIO $ parseRequest $ base ++ "/package-call"
        let request =
              (if isJust maybeToken
                 then (setRequestBasicAuth "n/a" (encodeUtf8 $ fromJust maybeToken))
                 else Prelude.id) $
              setRequestBodyJSON packageCallToLog $ initReq {method = "POST"}
        -- TODO - logging
        response <- httpBS request
        return $ ExecutionResult exitCode (fromIntegral runtime) safeArgString

redactArguments :: [Text] -> [Text]
redactArguments argList =
  foldl
    (\tokens currentArg ->
       if (("-" `T.isPrefixOf` (fromMaybe "" $ safeLast tokens)) && (not $ "-" `T.isPrefixOf` currentArg))
         then (tokens ++ ["<REDACTED_ARG>"])
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
      then (setRequestBasicAuth "n/a" (encodeUtf8 $ fromJust maybeToken))
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
      (return)
      (validateSpec parsedSpec)
  liftIO $ putStrLn "Uploading release"
  pPrint spec
  -- upload any archives
  let distrubutionsToUpload =
        filter (\dist -> not . isRemoteUrl $ PackageSpec.uri dist) $
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
        (setRequestBasicAuth "n/a" (encodeUtf8 token)) $
        initReq {method = "POST"}
  formified <- (formDataBody
                ([partFileRequestBody "spec" "spec.json" $ RequestBodyBS (L8.toStrict $ encode parsedSpec)] ++ archiveFileParts)
        request)
  response <- liftIO $ (flip Network.HTTP.Client.httpLbs) httpMgr formified
  if (getResponseStatusCode response) == 200
    then pPrint "Upload complete!" >> return ()
    else pPrint
           ("[" <> (show $ getResponseStatus response) <> "] Message: " <>
            (show $ response)) >>
         return ()

isRemoteUrl u =
  ("http://" `T.isPrefixOf` u) || ("https://" `T.isPrefixOf` u)

originalProgram homeFolder fileName = homeFolder <>  "/.scarf/original/" <> fileName

wrappedProgram homeFolder fileName = homeFolder <> "/.scarf/bin/" <> fileName

setUpScarfDirs ::
     (MonadReader Config m, MonadIO m, MonadThrow m) => m ()
setUpScarfDirs = do
  home <- asks homeDirectory
  liftIO $ createDirectoryIfMissing True (toString $ originalProgram home "")
  liftIO $ createDirectoryIfMissing True (toString $ wrappedProgram home "")

installProgramWrapped ::
     (MonadReader Config m, MonadIO m, MonadThrow m) => Text -> m ()
installProgramWrapped pkgName = do
  home <- asks homeDirectory
  base <- asks backendBaseUrl
  _ <- setUpScarfDirs
  manager' <- asks httpManager
  parsedBaseUrl <- parseBaseUrl base
  _details <- liftIO $ runClientM (askGetPackageDetails pkgName)  (mkClientEnv manager' parsedBaseUrl)
  case _details of
    Left servantErr -> throwM $ makeCliError servantErr
    Right details   -> do
      let maybeRelease = latestRelease hostPlatform details
      when (isNothing maybeRelease) $ throwM $ NotFoundError "No release found"
      let releaseToInstall = fromJust maybeRelease
      let fetchUrl = releaseToInstall ^. executableUrl
      let wrappedProgramPath = (toString $ wrappedProgram home pkgName)
      downloadAndInstallOriginal home releaseToInstall fetchUrl
      liftIO $
        writeFile
          wrappedProgramPath
          (T.unlines
             [ "#!/bin/bash"
             , "function join_by { local d=$1; shift; echo -n \"$1\"; shift; printf \"%s\" \"${@/#/$d}\"; }"
             , toText $ printf "arg_string=$(join_by \"%s\" \"$@\")" delimeter
             , toText $
               printf
                 "scarf execute %s%s%s --args \"$arg_string\""
                 (releaseToInstall ^. uuid)
                 delimeter
                 pkgName
             ])
      liftIO $ setFileMode wrappedProgramPath accessModes
      logPackageInstall (releaseToInstall ^. uuid)
      liftIO $ printf "Installation complete: %s\n" wrappedProgramPath

logPackageInstall :: (MonadReader Config m, MonadIO m, MonadThrow m) => Text -> m ()
logPackageInstall pkgUuid = do
  base <- asks backendBaseUrl
  request <-
    buildRequestWithTokenAuth
      (toText base <> "/package-event/install/" <> pkgUuid)
      "POST"
  response <- httpBS request
  if (getResponseStatusCode response) == 200
    then return ()
    else throwM $
         UnknownError
           ("[" <> (toText . show $ getResponseStatus response) <> "] Message: " <>
            (toText . show $ response))

latestRelease ::
     PackageSpec.Platform -> PackageDetails -> Maybe PackageRelease
latestRelease releasePlatform details =
  let maybeLatestVersion =
        safeHead . reverse . List.sort $
        map (^. version) $
        filter (\r -> r ^. platform == releasePlatform) (details ^. releases)
  in maybeLatestVersion >>=
     (\latestVersion ->
        List.find
          (\r ->
             r ^. platform == releasePlatform && r ^. version == latestVersion)
          (details ^. releases))

-- TODO(#error-handling) catch installation IO exceptions
downloadAndInstallOriginal ::
     (MonadIO m) => FilePath -> PackageRelease -> Text -> m ()
downloadAndInstallOriginal homeDir release url =
  let tmpArchive = "/tmp/tmp-u-package-install.tar.gz"
      tmpArchiveExtracedFolder = "/tmp/tmp-u-package-install"
      tmpExtractedBin =
        tmpArchiveExtracedFolder <> "/" <>
        (toString . fromJust $ release ^. simpleExecutableInstall)
      executableName = fromJust $ release ^. simpleExecutableInstall
      installPath =
        originalProgram
          homeDir
          ((release ^. uuid) <> delimeter <> executableName)
  in liftIO $ do
       putStrLn . toString $ "Downloading package " <> executableName
       request <- parseRequest $ T.unpack url
       downloadResp <- httpBS $ request
       _ <-
         L8.writeFile tmpArchive (L8.fromStrict $ getResponseBody downloadResp)
       putStrLn "Extracting..."
       Tar.unpack tmpArchiveExtracedFolder . Tar.read . GZ.decompress =<<
         L8.readFile tmpArchive
       putStrLn "Copying..."
       permissions <- liftIO $ getPermissions tmpExtractedBin
       setPermissions tmpExtractedBin (setOwnerExecutable True permissions)
       copyFile tmpExtractedBin $ toString installPath

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
  | T.isInfixOf "-" x || (T.isInfixOf "-" y) = compPerPart (T.splitOn "-" x) (T.splitOn "-" y)
  | otherwise = compare x y

makeCliError :: ServantError -> CliError
makeCliError s = CliConnectionError . toText $ show s

lintDhallPackageFile :: (MonadReader Config m, MonadIO m) => FilePath -> m (Either Text ValidatedPackageSpec)
lintDhallPackageFile f = do
  home <- asks homeDirectory
  let pathFixed = (T.replace "~" home f)
  (parsedPackage :: Either Text PackageSpec.PackageSpec) <- parseDhallEither pathFixed
  case parsedPackage of
    Left err -> do
      liftIO . putStrLn $ toString err
      return $ Left err
    result@(Right pkg) ->
      let validated = validateSpec pkg in
        either (liftIO . putStrLn . toString) (pPrint) validated >>
          return validated

parseDhallOrThrow :: (MonadIO m, Dhall.Interpret a, MonadThrow m) => FilePath -> m a
parseDhallOrThrow f =
  parseDhallEither f >>= \result ->
    either (throwM) (return) result

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

filterJustAndUnwrap = (map fromJust) . (filter isJust)

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
