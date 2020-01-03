{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module Scarf.Common where

import           Control.Exception.Safe     (Exception, MonadThrow,
                                             SomeException, throwM)
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Reader.Class
import           Data.Char
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Typeable
import           Distribution.Parsec.Class
import           Distribution.Version
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Prelude                    hiding (FilePath, writeFile)
import           System.Exit
import           System.Process

makeFieldLabelModfier :: String -> String -> String
makeFieldLabelModfier typeName = lowerFirst . (drop $ length typeName)
  where
    lowerFirst :: String -> String
    lowerFirst s =
      if null s
        then s
        else (toLower $ head s) : tail s

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeLast = safeHead . reverse

type FilePath = Text
type ExecutableId = Text
type Username = Text
type PackageName = Text

delimeter :: Text
delimeter = "----"

toString = T.unpack
toText = T.pack

data CliError
  = CliConnectionError Text
  | NotFoundError Text
  | NoCredentialsError
  | DhallError Text
  | PackageSpecError Text
  | PackageLookupError Text
  | UserStateCorrupt Text
  | UserError Text
  | MalformedVersion Text
  | PackageNotInstalled
  | ExternalInstallFailed Int
  | NothingToDo
  | PackageScriptFailed Int
  | InvalidSignature Text Text
  | UnknownError Text
  deriving (Typeable, Show)

instance Exception CliError
instance Exception Text

nothingToDoHandler :: MonadThrow m => CliError -> m ()
nothingToDoHandler exception = case exception of
  NothingToDo -> return ()
  other       -> throwM other

getJusts :: [Maybe a] -> [a]
getJusts = (map fromJust) . (filter isJust)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right b) = Right b

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Right a) = Right (f a)
mapRight _ (Left b)  = Left b

-- TODO(#techdebt) copying files by calling out to the shell is certainly not ideal
copyFileOrDir :: String -> String -> IO ExitCode
copyFileOrDir src dest = system $ "cp -r " ++ src ++ " " ++ dest

maybeListToList :: Maybe [a] -> [a]
maybeListToList Nothing   = []
maybeListToList (Just as) = as


putTextLn :: Text -> IO ()
putTextLn = putStrLn . Scarf.Common.toString

putTextLnM :: MonadIO m => Text -> m ()
putTextLnM = liftIO . putTextLn

filterJustAndUnwrap = Data.Maybe.catMaybes

-- Try to parse a regular version, then convert to range. If that fails, parse a range
parseVersionRange :: Text -> Either String VersionRange
parseVersionRange t =
  let bareVersion = eitherParsec $ toString t in
    either (const $ eitherParsec $ toString t) (Right . thisVersion) bareVersion

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  concat <$> mapM f xs

eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a

addOrReplace :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
addOrReplace key value assoc = (key,value):(filter ((key /=).fst) assoc)

addOrReplaceOn :: Eq k => (k -> Bool) -> k -> v -> [(k, v)] -> [(k, v)]
addOrReplaceOn f key value assoc = (key,value):(filter (not . f . fst) assoc)

stripQuotes :: String -> String
stripQuotes s =
  if (length s > 0) && ('"' == head s) && ('"' == last s)
    then tail $ init s
    else s

truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

orThrow :: (MonadThrow m, Exception e) => Maybe a -> e -> m a
orThrow a b = maybe (throwM b) return a

orThrowM :: (MonadThrow m, Exception e) => m (Maybe a) -> e -> m a
orThrowM a b = a >>= (\_a -> maybe (throwM b) return _a)

stringToBool :: String -> Bool
stringToBool ""      = False
stringToBool "0"     = False
stringToBool "false" = False
stringToBool "False" = False
stringToBool _       = True

data Config = Config
  { homeDirectory  :: FilePath
  , userApiToken   :: Maybe Text
  , httpManager    :: Manager
  , backendBaseUrl :: String
  , useSudo        :: Bool
  , cliDebug       :: Bool
  }

type ScarfContext m = (MonadReader Config m, MonadIO m, MonadThrow m, MonadCatch m)
type IOConfigContext m = (MonadReader Config m, MonadIO m)
