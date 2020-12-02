{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Scarf.Api where

import           Scarf.Common
import           Scarf.PackageSpec
import           Scarf.Types

import           Data.Kind (Type)
import           Data.Text (Text)
import           Servant
import           Servant.Auth.Server

type Get302 (cts :: [Type]) (hs :: [Type]) =
  Verb 'GET 302 cts (Headers (Header "Location" Text ': hs) NoContent)

type OpenAPI = "user" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] CreateUserResponse)
            :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)
            :<|> "login-github" :> Get302 '[JSON] '[]
            :<|> "login-github-callback"
               :> QueryParam "code" Text
               :> QueryParam "state" Text
               :> Get302 '[JSON] '[ Header "Set-Cookie" SetCookie
                                  , Header "Set-Cookie" SetCookie ]
            :<|> "login-github-link-account"
               :> ReqBody '[JSON] LoginRequest :> Post '[JSON]
               (Headers '[ Header "Set-Cookie" SetCookie
                         , Header "Set-Cookie" SetCookie] NoContent)
            :<|> "clear-session" :> Get '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)
            :<|> "package"
               :> Capture "package" PackageName
               :> QueryParam "external_library_type" Scarf.PackageSpec.ExternalLibraryType
               :> QueryParam "owner_name" Text
               :> Get '[JSON] PackageDetails
            :<|> "packages" :> "index"
                 :> ReqBody '[JSON] LatestPackageIndexRequest
                 :> Post '[JSON] LatestPackageIndex
            :<|> "packages" :> "search" :> Capture "package" PackageName :> Get '[JSON] PackageSearchResults
            :<|> "cli-version" :> Get '[JSON] CliVersionResponse
            :<|> "feedback" :> ReqBody '[JSON] FeedbackRequest :> Post '[JSON] NoContent

openApiProxy :: Proxy OpenAPI
openApiProxy = Proxy
