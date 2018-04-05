module Handler.BotEntity where

import Control.Monad.Persist
import Yesod.Core.Handler (notAuthenticated, permissionDenied)

import Import hiding (delete, deleteBy, selectFirst, selectList, mapM_)
import qualified DB
import Util
import HandlerUtil

postBotEntityDeleteR :: Text -> Handler Html
postBotEntityDeleteR appId = do
  muser <- getAuthUserPublic
  case muser of
    Nothing -> notAuthenticated
    Just userId -> do
      muserxapp <- selectFirst [exact DB.UserXBotUser userId, exact DB.UserXBotBot appId] []
      case muserxapp of
        Nothing -> permissionDenied "Unauthorized"
        Just (Entity key val) -> do
          apiKeys <- selectList [exact DB.ApiKeyBot (DB.userXBotBot val)] []
          mapM_ (\(Entity key _) -> delete key) apiKeys
          delete key
          deleteBy $ DB.UniqueBotPublic (DB.userXBotBot val)
  redirect DeveloperR
