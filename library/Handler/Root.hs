{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Root where

import Import

getHomeR :: Handler Html
getHomeR = do
  -- sess <- getSession
  defaultLayout $ do
    aDomId <- newIdent
    setTitle "AI {COMPO}"
    $(widgetFile "homepage")
