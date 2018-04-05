module Handler.App where

import Yesod.Core.Handler (notAuthenticated)
import Control.Monad.Persist
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import qualified DB
import Import hiding (selectFirst)
import App.ApiKey
import HandlerUtil
import Util
import Handler.Developer

postAppR :: Handler Html
postAppR = do
  muser <- getAuthUserPublic
  ((appFormResult, _), _) <- runFormPost appForm
  let submission = case appFormResult of
        FormSuccess res -> Just res
        _ -> Nothing
  case muser of
    Nothing -> notAuthenticated
    Just userId -> do
      case submission of
        Nothing -> return ()
        Just (AppForm appName) -> insertApp userId appName
  redirect DeveloperR
