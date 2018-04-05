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
module AiCompo.TicTacToe.Api.Server
  ( ticTacToe'handlerMap
  , ticTacToe'spec
  , V0.AccessToken(..)
  , V0.UserId(..)
  , V0.Group(..)
  , V0.GameId(..)
  , V0.GameToken(..)
  , V0.Board(..)
  , V0.Loc(..)
  , V0.State(..)
  , V0.Users(..)
  , V0.Init(..)
  , V0.Frame(..)
  , V0.Playback(..)
  , V0.PostMove(..)
  , V0.GetPlayback(..)
  , V0.Player(..)
  , V0.Error(..)
  , V0.Final(..)
  , V0.Result(..)
  , V0.TicTacToe'Service(..)
  , V0.TicTacToe'Thrower(..)
  , V0.ticTacToe'pull
  ) where

import qualified Prelude as P
import qualified Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)
import qualified Fluid.Imports as R
import qualified AiCompo.TicTacToe.Api.Major0 as V0
  ( TicTacToe'Service(..)
  , TicTacToe'Thrower(..)
  , ticTacToe'handler
  , ticTacToe'version
  , ticTacToe'pull
  , ticTacToe'spec
  , AccessToken(..)
  , UserId(..)
  , Group(..)
  , GameId(..)
  , GameToken(..)
  , Board(..)
  , Loc(..)
  , State(..)
  , Users(..)
  , Init(..)
  , Frame(..)
  , Playback(..)
  , PostMove(..)
  , GetPlayback(..)
  , Player(..)
  , Error(..)
  , Final(..)
  , Result(..)
  )

ticTacToe'handlerMap
  ::
    ( R.MonadIO m
    , R.MonadCatch m
    , V0.TicTacToe'Service meta0 m
    )
  => (xtra -> C.Hooks m V0.AccessToken meta0)
  -> xtra
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
ticTacToe'handlerMap hooks0 xtra = R.fromList
    [ (0, (0, V0.ticTacToe'handler hooks0 xtra))
    ]

ticTacToe'spec :: R.Value
ticTacToe'spec = R.toJSON
  [ V0.ticTacToe'spec
  ]
