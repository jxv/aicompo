module Handler.AppEntity where

import Control.Monad.Persist
import Control.Monad.Trans.Maybe
import Yesod.Core.Handler (notAuthenticated, permissionDenied)

import Import hiding (delete, deleteBy, selectFirst, selectList, mapM_)
import qualified DB
import Util
import HandlerUtil

postAppEntityDeleteR :: Text -> Handler Html
postAppEntityDeleteR appId = do
  muser <- getAuthUserPublic
  case muser of
    Nothing -> notAuthenticated
    Just userId -> do
      muserxapp <- selectFirst [exact DB.UserXAppUser userId, exact DB.UserXAppApp appId] []
      case muserxapp of
        Nothing -> permissionDenied "Unauthorized"
        Just (Entity key val) -> do
          apiKeys <- selectList [exact DB.ApiKeyApp (DB.userXAppApp val)] []
          mapM_ (\(Entity key _) -> delete key) apiKeys
          delete key
          deleteBy $ DB.UniqueAppPublic (DB.userXAppApp val)
  redirect DeveloperR
