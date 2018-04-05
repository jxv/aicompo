{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handler.ApiTicTacToe where

import Fluid.Server
import Fluid.Endpoint
import Control.Monad.Persist

import qualified DB
import qualified AiCompo.TicTacToe.Api.Server as Api
import qualified AiCompo.TicTacToe.Api.Major0 as V0
import qualified AiCompo.TicTacToe.Service as TS
import Import hiding (selectFirst)
import Util (exact)

getApiTicTacToeR :: Handler Value
getApiTicTacToeR = return Api.ticTacToe'spec

postApiTicTacToeR :: Handler Value
postApiTicTacToeR = do
  v <- (requireJsonBody :: Handler Value)
  let handlerMap = Api.ticTacToe'handlerMap
        (const $ Hooks
          { metaMiddleware = ticTacToeMetaMiddleware0
          , sandboxLimits = \_ -> return $ defLimits { serviceCalls = Just 1 }
          })
        ()
  runFluid handlerMap v

ticTacToeMetaMiddleware0 :: V0.Meta -> Handler (Maybe TS.BotId)
ticTacToeMetaMiddleware0 (V0.Meta Nothing) = return Nothing
ticTacToeMetaMiddleware0 (V0.Meta (Just (V0.ApiKey apiKey))) = do
  mApikey <- selectFirst [exact DB.ApiKeyKey apiKey] []
  case mApikey of
    Nothing -> return Nothing
    Just (Entity _ DB.ApiKey{DB.apiKeyBot}) -> return $ Just $ TS.BotId apiKeyBot

instance V0.TicTacToe'Thrower Handler

instance V0.TicTacToe'Service (Maybe TS.BotId) Handler where
  ticTacToe'PostStart meta = asks appTicTacToeComponents >>= (\t -> TS.postStart t meta)
  ticTacToe'PostMove meta req = asks appTicTacToeComponents >>= (\t -> TS.postMove t meta req)
  ticTacToe'GetPlayback meta req = asks appTicTacToeComponents >>= (\t -> TS.getPlayback t meta req)
