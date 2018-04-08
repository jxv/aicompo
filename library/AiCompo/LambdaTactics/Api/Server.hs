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
module AiCompo.LambdaTactics.Api.Server
  ( lambdaTactics'handlerMap
  , lambdaTactics'spec
  , V0.ApiKey(..)
  , V0.BotId(..)
  , V0.Group(..)
  , V0.GameId(..)
  , V0.GameToken(..)
  , V0.Meta(..)
  , V0.Board(..)
  , V0.Move(..)
  , V0.State(..)
  , V0.Bots(..)
  , V0.Init(..)
  , V0.Frame(..)
  , V0.Playback(..)
  , V0.PostMove(..)
  , V0.GetPlayback(..)
  , V0.Player(..)
  , V0.Error(..)
  , V0.Terrain(..)
  , V0.Terrain'City'Members(..)
  , V0.Final(..)
  , V0.Result(..)
  , V0.LambdaTactics'Service(..)
  , V0.LambdaTactics'Thrower(..)
  , V0.lambdaTactics'pull
  ) where

import qualified Prelude as P
import qualified Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)
import qualified Fluid.Imports as R
import qualified AiCompo.LambdaTactics.Api.Major0 as V0
  ( LambdaTactics'Service(..)
  , LambdaTactics'Thrower(..)
  , lambdaTactics'handler
  , lambdaTactics'version
  , lambdaTactics'pull
  , lambdaTactics'spec
  , ApiKey(..)
  , BotId(..)
  , Group(..)
  , GameId(..)
  , GameToken(..)
  , Meta(..)
  , Board(..)
  , Move(..)
  , State(..)
  , Bots(..)
  , Init(..)
  , Frame(..)
  , Playback(..)
  , PostMove(..)
  , GetPlayback(..)
  , Player(..)
  , Error(..)
  , Terrain(..)
  , Terrain'City'Members(..)
  , Final(..)
  , Result(..)
  )

lambdaTactics'handlerMap
  ::
    ( R.MonadIO m
    , R.MonadCatch m
    , V0.LambdaTactics'Service meta0 m
    )
  => (xtra -> C.Hooks m V0.Meta meta0)
  -> xtra
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
lambdaTactics'handlerMap hooks0 xtra = R.fromList
    [ (0, (0, V0.lambdaTactics'handler hooks0 xtra))
    ]

lambdaTactics'spec :: R.Value
lambdaTactics'spec = R.toJSON
  [ V0.lambdaTactics'spec
  ]
