module Handler.ClientCredentialsEntity where

import Control.Monad.Persist
import Control.Monad.Trans.Maybe
import Yesod.Core.Handler (notAuthenticated, permissionDenied)

import Import hiding (delete, selectFirst)
import qualified DB
import Util
import HandlerUtil

postClientCredentialsEntityDeleteR :: Text -> Handler Html
postClientCredentialsEntityDeleteR clientId = do
  muser <- getAuthUserPublic
  case muser of
    Nothing -> notAuthenticated
    Just user -> do
      mcc <- selectFirst [exact DB.ClientCredentialsUser user, exact DB.ClientCredentialsClientId clientId] []
      case mcc of
        Nothing -> permissionDenied "Unauthorized"
        Just (Entity key val) ->
          if DB.clientCredentialsUser val == user
            then delete key
            else permissionDenied "Unauthorized"
  redirect DeveloperR
