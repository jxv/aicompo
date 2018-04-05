module Handler.ApiKeyEntity where

import Control.Monad.Persist
import Control.Monad.Trans.Maybe
import Yesod.Core.Handler (notAuthenticated, permissionDenied)

import Import hiding (delete, selectFirst)
import qualified DB
import Util
import HandlerUtil

postApiKeyEntityDeleteR :: Text -> Text -> Handler Html
postApiKeyEntityDeleteR appId apiKey = do
  muser <- getAuthUserPublic
  case muser of
    Nothing -> notAuthenticated
    Just userId -> do
      muserxapp <- selectFirst [exact DB.UserXAppUser userId, exact DB.UserXAppApp appId] []
      case muserxapp of
        Nothing -> permissionDenied "Unauthorized"
        Just (Entity _ val) -> do
          mapikey <- selectFirst [exact DB.ApiKeyApp (DB.userXAppApp val), exact DB.ApiKeyKey apiKey] []
          case mapikey of
            Nothing -> permissionDenied "Unauthorized"
            Just (Entity key' _) -> delete key'
  redirect DeveloperR
