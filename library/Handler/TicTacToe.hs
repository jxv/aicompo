{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.TicTacToe where

import qualified DB
import Control.Monad.Persist (MonadPersist(selectList))
import Database.Persist.Types (SelectOpt(..))
import Import hiding (selectList)
import Util

getTicTacToeR :: Handler Html
getTicTacToeR = do
  playbackEntities <- selectList [] [Desc DB.TicTacToePlaybackCreated]
  let playbacks = map entityVal playbackEntities
  defaultLayout $ do
    setTitle "AI {COMPO}"
    $(widgetFile "tictactoe")
