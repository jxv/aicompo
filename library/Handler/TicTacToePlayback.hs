{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.TicTacToePlayback where

import qualified Fluid.ServiceThrower as Fluid
import qualified AiCompo.TicTacToe.Service as TS
import qualified AiCompo.TicTacToe.Api.Server as T
import Import
import Handler.ApiTicTacToe ()

getTicTacToePlaybackR :: Text -> Handler Html
getTicTacToePlaybackR gamePublic = do
  let gameId = T.GameId gamePublic
  maybePlayback <- Fluid.mayServiceThrow (T.ticTacToe'GetPlayback (Nothing :: Maybe TS.BotId) (T.GetPlayback gameId))
  defaultLayout $ do
    setTitle "AI {COMPO}"
    $(widgetFile "tictactoe-playback")

showResult :: T.Result -> Text
showResult = \case
  T.Result'WinnerX -> "Winner X"
  T.Result'WinnerO -> "Winner O"
  T.Result'Tie -> "Tie"
