{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Scarf.Client where

import           Scarf.Api
import           Scarf.PackageSpec

import           Servant
import           Servant.Client

askCreateUser :<|>
  askLogIn :<|>
  askLogInGitHub :<|>
  askLogInGitHubCallback :<|>
  askLogInGitHubLinkAccount :<|>
  askClearSession :<|>
  askGetPackageDetails :<|>
  askGetPackageIndex :<|>
  askSearchPackages :<|>
  askGetCurrentCliVersion :<|>
  askSendFeedback =
  client openApiProxy
