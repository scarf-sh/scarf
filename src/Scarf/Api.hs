{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Scarf.Api where

import           Scarf.Common
import           Scarf.PackageSpec
import           Scarf.Types


import           Servant
import           Servant.Auth.Server

type OpenAPI = "user" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] CreateUserResponse)
            :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)
            :<|> "clear-session" :> Get '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)
            :<|> "package" :> Capture "package" PackageName :> QueryParam "external_library_type" Scarf.PackageSpec.ExternalLibraryType :> Get '[JSON] PackageDetails
            :<|> "packages" :> "index"
                 :> ReqBody '[JSON] LatestPackageIndexRequest
                 :> Post '[JSON] LatestPackageIndex
            :<|> "packages" :> "search" :> Capture "package" PackageName :> Get '[JSON] PackageSearchResults
            :<|> "cli-version" :> Get '[JSON] CliVersionResponse
            :<|> "feedback" :> ReqBody '[JSON] FeedbackRequest :> Post '[JSON] NoContent

openApiProxy :: Proxy OpenAPI
openApiProxy = Proxy
