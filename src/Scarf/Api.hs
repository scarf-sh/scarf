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

import           Data.Text           (Text)
import           Servant
import           Servant.Auth.Server

type OpenAPI = "user" :> ReqBody '[JSON] CreateUserRequest :> Post '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)

            :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)

            :<|> "clear-session" :> Get '[JSON]
              (Headers '[ Header "Set-Cookie" SetCookie
              , Header "Set-Cookie" SetCookie] NoContent)

            :<|> "package" :> Capture "package" PackageName :> Get '[JSON] PackageDetails
            :<|> "packages" :> "search" :> Capture "package" PackageName :> Get '[JSON] PackageSearchResults

openApiProxy :: Proxy OpenAPI
openApiProxy = Proxy
