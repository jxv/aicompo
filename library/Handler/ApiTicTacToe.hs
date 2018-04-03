{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.ApiTicTacToe where

import Fluid.Server
import Fluid.Endpoint

import qualified AiCompo.TicTacToe.Api.Server as Api
import qualified AiCompo.TicTacToe.Api.Major0 as V0
import qualified AiCompo.TicTacToe.Service as TS
import Import
import AiCompo.User

import OAuth2.ClientCredentials

import Import

getApiTicTacToeR :: Handler Value
getApiTicTacToeR = return Api.ticTacToe'spec

postApiTicTacToeR :: Handler Value
postApiTicTacToeR = do
  v <- (requireJsonBody :: Handler Value)
  let handlerMap = Api.ticTacToe'handlerMap (const $ defHooks {metaMiddleware = ticTacToeMetaMiddleware0}) ()
  runFluid handlerMap v

ticTacToeMetaMiddleware0 :: V0.AccessToken -> Handler TS.UserId
ticTacToeMetaMiddleware0 (V0.AccessToken accessToken') = return $ TS.UserId accessToken'

instance V0.TicTacToe'Thrower Handler

instance V0.TicTacToe'Service TS.UserId Handler where
  ticTacToe'PostStart meta = asks appTicTacToeComponents >>= (\t -> TS.postStart t meta)
  ticTacToe'PostMove meta req = asks appTicTacToeComponents >>= (\t -> TS.postMove t meta req)
