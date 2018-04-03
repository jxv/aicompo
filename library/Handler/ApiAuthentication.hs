{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.ApiAuthentication where

import Fluid.Server
import Fluid.Endpoint

import qualified AiCompo.Authentication.Api.Server as Api
import qualified AiCompo.Authentication.Api.Major0 as V0
import Import
import AiCompo.User
import OAuth2.ClientCredentials

getApiAuthenticationR :: Handler Value
getApiAuthenticationR = return Api.api'spec

postApiAuthenticationR :: Handler Value
postApiAuthenticationR = do
  v <- (requireJsonBody :: Handler Value)
  let handlerMap = Api.api'handlerMap (\() -> defHooks) ()
  runFluid handlerMap v

instance V0.Api'Service () Handler where
  api'GetUser = getUser
  api'GetUserCount = getUserCount
  api'GetClientCredentials = getClientCredentials
  api'DeleteClientCredentials = deleteClientCredentials
  api'PostClientCredentialsGrant = postClientCredentialsGrant
