{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Api where

import           Lib
import qualified Models                                   as DB
import qualified PackageSpec                              as PackageSpec

import           Control.Monad.Reader                     (MonadReader, ReaderT,
                                                           ask, asks,
                                                           runReaderT)
import           Crypto.BCrypt
import           Crypto.JOSE.JWK
import           Data.Aeson                               (FromJSON, ToJSON)
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.ByteArray.Encoding
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Char8                    as B8S
import qualified Data.ByteString.Lazy                     as BSL
import           Data.Foldable
import           Data.Maybe
import           Data.Pool
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           Data.Text.Encoding
import           Data.Time.Clock
import           Database.Beam
import qualified Database.Beam.Backend.SQL.BeamExtensions as Extensions
import           Database.Beam.Postgres
import           Lens.Micro.Platform
import           Network.Wai.Handler.Warp                 (run)
import           Servant
import           Servant.Auth.Server
import           Servant.Client
import qualified Servant.Client.Streaming                 as S
import           Servant.HTML.Blaze                       (HTML)
import           Text.Blaze.Html5                         (Html)
import qualified Text.Blaze.Html5                         as BZ
import           Text.Pretty.Simple
import           Text.Printf


type StaticAPI = "static" :> Raw

            :<|> CaptureAll "anything-else" Text :> Get '[HTML] Html

type OpenAPI = "user" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)

            :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)

            :<|> "package" :> Capture "package" PackageName :> Get '[JSON] PackageDetails

type OptionallyProtectedAPI =
  "package-call" :> ReqBody '[JSON] CreatePackageCallRequest :> Post '[JSON] NoContent

type ProtectedAPI = "logged-in" :> Get '[JSON] Session
  :<|> "package" :> ReqBody '[JSON] CreatePackageRequest :> Post '[JSON] NoContent
  :<|> "package" :> "release" :> ReqBody '[JSON] CreatePackageReleaseRequest :> Post '[JSON] NoContent
  :<|> "packages" :> Get '[JSON] GetPackagesResponse

type FullAPI auths = (Auth auths Session :> ProtectedAPI) :<|> (Auth auths Session :> OptionallyProtectedAPI) :<|> OpenAPI :<|> StaticAPI

openApiProxy :: Proxy OpenAPI
openApiProxy = Proxy

fullAPI :: Proxy (FullAPI '[Cookie, JWT, Servant.Auth.Server.BasicAuth])
fullAPI = Proxy
