module Handler.ApiKeyEntity where

import Control.Monad.Persist
import Yesod.Core.Handler (notAuthenticated, permissionDenied)

import Import hiding (delete, selectFirst)
import qualified DB
import Util
import HandlerUtil

postApiKeyEntityDeleteR :: Text -> Text -> Handler Html
postApiKeyEntityDeleteR botId apiKey = do
  muser <- getAuthUserPublic
  case muser of
    Nothing -> notAuthenticated
    Just userId -> do
      muserxbot <- selectFirst [exact DB.UserXBotUser userId, exact DB.UserXBotBot botId] []
      case muserxbot of
        Nothing -> permissionDenied "Unauthorized"
        Just (Entity _ val) -> do
          mapikey <- selectFirst [exact DB.ApiKeyBot (DB.userXBotBot val), exact DB.ApiKeyKey apiKey] []
          case mapikey of
            Nothing -> permissionDenied "Unauthorized"
            Just (Entity key' _) -> delete key'
  redirect DeveloperR
