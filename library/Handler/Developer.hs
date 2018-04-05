{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Developer where

import Import hiding (selectList)
import Control.Monad.Persist
import Control.Monad.Trans.Maybe
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import qualified DB
import Util
import HandlerUtil

data BotForm = BotForm
  { botFormName :: Text
  }

getDeveloperR :: Handler Html
getDeveloperR = do
  (botFormWidget, botFormEnctype) <- generateFormPost botForm
  mBotData <- runMaybeT $ do
    userId <- MaybeT getAuthUserPublic
    userXBots <- lift $ selectList [exact DB.UserXBotUser userId] []
    let botPublics = map (DB.userXBotBot . entityVal) userXBots
    bots <- lift $ mapM (\botPublic -> selectList [exact DB.BotPublic botPublic] []) botPublics
    flip mapM (concat bots) $ \(Entity _ bot) -> do
      apiKeys <- lift $ selectList [exact DB.ApiKeyBot (DB.botPublic bot)] []
      return (DB.botPublic bot, DB.botName bot, map (DB.apiKeyKey . entityVal) apiKeys)
  defaultLayout $ do
    setTitle "AI {COMPO}"
    $(widgetFile "developer")

botForm :: Form BotForm
botForm = renderBootstrap3 BootstrapBasicForm $ BotForm
    <$> areq textField textSettings Nothing
    where textSettings = FieldSettings
            { fsLabel = ""
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control app-form-name")
                , ("placeholder", "Bot Name")
                ]
            }
