module Handler.ApiKey where

import Yesod.Core.Handler (notAuthenticated)
import Control.Monad.Persist

import qualified DB
import Import hiding (selectFirst)
import AiCompo.Bot
import HandlerUtil
import Util

postApiKeyR :: Text -> Handler Html
postApiKeyR botPublic = do
  muser <- getAuthUserPublic
  case muser of
    Nothing -> notAuthenticated
    Just userId -> do
      muserxapp <- selectFirst [exact DB.UserXBotUser userId, exact DB.UserXBotBot botPublic] []
      case muserxapp of
        Nothing -> permissionDenied "Unauthorized"
        Just _ -> do
          insertApiKey botPublic
  redirect DeveloperR
