module Handler.ApiKey where

import Yesod.Core.Handler (notAuthenticated)
import Control.Monad.Persist

import qualified DB
import Import hiding (selectFirst)
import App.ApiKey
import HandlerUtil
import Util

postApiKeyR :: Text -> Handler Html
postApiKeyR appPublic = do
  muser <- getAuthUserPublic
  case muser of
    Nothing -> notAuthenticated
    Just userId -> do
      muserxapp <- selectFirst [exact DB.UserXAppUser userId, exact DB.UserXAppApp appPublic] []
      case muserxapp of
        Nothing -> permissionDenied "Unauthorized"
        Just _ -> do
          insertApiKey appPublic
  redirect DeveloperR
