module Handler.ClientCredentials where

import Yesod.Core.Handler (notAuthenticated)

import Import
import OAuth2.ClientCredentials
import HandlerUtil

postClientCredentialsR :: Handler Html
postClientCredentialsR = do
  muser <- getAuthUserPublic
  case muser of
    Nothing -> notAuthenticated
    Just user -> insertClientCredentials user
  redirect DeveloperR
