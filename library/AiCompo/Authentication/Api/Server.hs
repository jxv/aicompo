-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module
module AiCompo.Authentication.Api.Server
  ( api'handlerMap
  , api'spec
  , V0.Seconds(..)
  , V0.UserId(..)
  , V0.Email(..)
  , V0.AccessToken(..)
  , V0.ClientId(..)
  , V0.ClientSecret(..)
  , V0.Date(..)
  , V0.User(..)
  , V0.GetUser(..)
  , V0.ClientCredentials(..)
  , V0.ClientCredentialsGrant(..)
  , V0.GetClientCredentials(..)
  , V0.PostClientCredentialsGrant(..)
  , V0.DeleteClientCredentials(..)
  , V0.Role(..)
  , V0.Api'Service(..)
  , V0.Api'Thrower(..)
  , V0.api'pull
  ) where

import qualified Prelude as P
import qualified Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)
import qualified Fluid.Imports as R
import qualified AiCompo.Authentication.Api.Major0 as V0
  ( Api'Service(..)
  , Api'Thrower(..)
  , api'handler
  , api'version
  , api'pull
  , api'spec
  , Seconds(..)
  , UserId(..)
  , Email(..)
  , AccessToken(..)
  , ClientId(..)
  , ClientSecret(..)
  , Date(..)
  , User(..)
  , GetUser(..)
  , ClientCredentials(..)
  , ClientCredentialsGrant(..)
  , GetClientCredentials(..)
  , PostClientCredentialsGrant(..)
  , DeleteClientCredentials(..)
  , Role(..)
  )

api'handlerMap
  ::
    ( R.MonadIO m
    , R.MonadCatch m
    , V0.Api'Service meta0 m
    )
  => (xtra -> C.Hooks m () meta0)
  -> xtra
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
api'handlerMap hooks0 xtra = R.fromList
    [ (0, (0, V0.api'handler hooks0 xtra))
    ]

api'spec :: R.Value
api'spec = R.toJSON
  [ V0.api'spec
  ]
