{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Api where

import           Common
import           PackageSpec
import           Types

import           Data.Text           (Text)
import           Servant
import           Servant.Auth.Server
import           Servant.HTML.Blaze  (HTML)
import           Servant.Multipart
import           Text.Blaze.Html5    (Html)


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
  :<|> "package" :> "release" :>
                  MultipartForm Mem (MultipartData Mem) :>
                  Post '[JSON] NoContent
  :<|> "package" :> "stats" :> Capture "package" PackageName :> Get '[JSON] PackageStatsResponse
  :<|> "packages" :> Get '[JSON] GetPackagesResponse
  :<|> "user" :> "account" :> Get '[JSON] GetUserAccountDetailsResponse
  :<|> "user" :> "at" :> Post '[JSON] GetUserAccountDetailsResponse
  :<|> "user" :> "password" :> ReqBody '[JSON] UpdatePasswordRequest :> Post '[JSON] NoContent

type FullAPI auths = (Auth auths Session :> ProtectedAPI) :<|> (Auth auths Session :> OptionallyProtectedAPI) :<|> OpenAPI :<|> StaticAPI

openApiProxy :: Proxy OpenAPI
openApiProxy = Proxy

fullAPI :: Proxy (FullAPI '[Cookie, JWT, Servant.Auth.Server.BasicAuth])
fullAPI = Proxy
