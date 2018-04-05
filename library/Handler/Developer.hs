{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Developer where

import Import hiding (selectList)
import Control.Monad.Persist
import Control.Monad.Trans.Maybe
import qualified DB
import Util
import HandlerUtil

getDeveloperR :: Handler Html
getDeveloperR = do
  mAppData <- runMaybeT $ do
    userId <- MaybeT getAuthUserPublic
    userXApps <- lift $ selectList [exact DB.UserXAppUser userId] []
    let appPublics = map (DB.userXAppApp . entityVal) userXApps
    apps <- lift $ mapM (\appPublic -> selectList [exact DB.AppPublic appPublic] []) appPublics
    flip mapM (concat apps) $ \(Entity _ app) -> do
      apiKeys <- lift $ selectList [exact DB.ApiKeyApp (DB.appPublic app)] []
      return (DB.appPublic app, DB.appName app, map (DB.apiKeyKey . entityVal) apiKeys)
  defaultLayout $ do
    setTitle "AI {COMPO}"
    $(widgetFile "developer")
