module Handler.Bot where

import Yesod.Core.Handler (notAuthenticated)
import Control.Monad.Persist
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import qualified DB
import Import hiding (selectFirst)
import AiCompo.Bot
import HandlerUtil
import Util
import Handler.Developer

postBotR :: Handler Html
postBotR = do
  muser <- getAuthUserPublic
  ((botFormResult, _), _) <- runFormPost botForm
  let submission = case botFormResult of
        FormSuccess res -> Just res
        _ -> Nothing
  case muser of
    Nothing -> notAuthenticated
    Just userId -> do
      case submission of
        Nothing -> return ()
        Just (BotForm botName) -> insertBot userId botName
  redirect DeveloperR
