{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Spa where

import Import
import Settings.StaticFiles

getSpaR :: Handler Html
getSpaR = defaultLayout $ do
  setTitle "AiCompo"
  addScript $ StaticR js_spa_js
  $(widgetFile "spa")
