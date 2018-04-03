{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Developer where

import Import
import Control.Monad.Trans.Maybe
import qualified DB
import Util
import HandlerUtil

getDeveloperR :: Handler Html
getDeveloperR = do
  mcreds <- runMaybeT $ do
    userId <- MaybeT getAuthUserPublic
    dbCreds <- lift $ runDB $ selectList [exact DB.ClientCredentialsUser userId] []
    pure $ pamf (map entityVal dbCreds) $ \c -> (DB.clientCredentialsClientId c, DB.clientCredentialsClientSecret c)
  defaultLayout $ do
    setTitle "AI {COMPO}"
    $(widgetFile "developer")
