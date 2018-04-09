{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.TicTacToe where

import Import

getTicTacToeR :: Handler Html
getTicTacToeR = do
  defaultLayout $ do
    setTitle "AI {COMPO}"
    $(widgetFile "tictactoe")
