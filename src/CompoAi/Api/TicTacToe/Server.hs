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
module CompoAi.Api.TicTacToe.Server
  ( ticTacToe'handlerMap
  , ticTacToe'spec
  , ticTacToe'Scotty'Post
  , ticTacToe'Scotty'Get
  , V0.Meta(..)
  , V0.Error(..)
  , V0.Hello(..)
  , V0.TicTacToe'Service(..)
  , V0.TicTacToe'Thrower(..)
  , V0.ticTacToe'pull
  ) where

import qualified Prelude as P
import qualified Fluid.Server as C (RuntimeThrower, Hooks, Request, Response, Major, Minor, Pull)
import qualified Fluid.Imports as R
import qualified Fluid.Server.Scotty as Scotty
import qualified CompoAi.Api.TicTacToe.Major0 as V0
  ( TicTacToe'Service(..)
  , TicTacToe'Thrower(..)
  , ticTacToe'handler
  , ticTacToe'version
  , ticTacToe'pull
  , ticTacToe'spec
  , Meta(..)
  , Error(..)
  , Hello(..)
  )

ticTacToe'handlerMap
  ::
    ( R.MonadIO m
    , R.MonadCatch m
    , V0.TicTacToe'Service meta0 m
    )
  => (xtra -> C.Hooks m V0.Meta meta0)
  -> xtra
  -> R.Map C.Major (C.Minor, C.Request -> m (P.Either C.Response C.Response))
ticTacToe'handlerMap hooks0 xtra = R.fromList
    [ (0, (0, V0.ticTacToe'handler hooks0 xtra))
    ]

ticTacToe'spec :: R.Value
ticTacToe'spec = R.toJSON
  [ V0.ticTacToe'spec
  ]

ticTacToe'Scotty'Post
  ::
    ( Scotty.ScottyError e
    , R.MonadIO m
    , R.MonadCatch m
    , V0.TicTacToe'Service meta0 m
    )
  => C.Pull
  -> ([(Scotty.LazyText, Scotty.LazyText)] -> C.Hooks m V0.Meta meta0)
  -> Scotty.ScottyT e m ()
ticTacToe'Scotty'Post pull hooks0 = Scotty.respond pull (ticTacToe'handlerMap hooks0)

ticTacToe'Scotty'Get :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
ticTacToe'Scotty'Get = Scotty.getSpec ticTacToe'spec
