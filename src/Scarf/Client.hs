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

import           Servant
import           Servant.Client

askCreateUser :<|> askLogIn :<|> askClearSession :<|> askGetPackageDetails :<|> askGetPackageIndex :<|> askSearchPackages :<|> askGetCurrentCliVersion =
  client openApiProxy
