module OAuth2.ClientCredentials where

import Data.Time
import Data.Text.Conversions (toText)
import Control.Monad.Persist

import qualified AiCompo.Authentication.Api.Major0 as V0
import qualified DB
import Import (Handler, Text, liftIO)
import Util
import HandlerUtil

generateClientId :: IO Text
generateClientId = generateText64 48

generateClientSecret :: IO Text
generateClientSecret = generateText64 80

insertClientCredentials :: Text -> Handler ()
insertClientCredentials userPublic = do
  now <- liftIO getCurrentTime
  clientId <- liftIO generateClientId
  clientSecret <- liftIO generateClientSecret
  insert_ (DB.ClientCredentials now userPublic clientId clientSecret)

getClientCredentials :: MonadPersist SqlBackend m => () -> V0.GetClientCredentials -> m [V0.ClientCredentials]
getClientCredentials () req = do
  entityCreds <- selectList [exact DB.ClientCredentialsUser (toText $ V0.getClientCredentialsUserId req)] []
  return . pamf entityCreds $ \Entity{entityVal=cc} -> V0.ClientCredentials
    { V0.clientCredentialsClientId = V0.ClientId (DB.clientCredentialsClientId cc)
    , V0.clientCredentialsClientSecret = V0.ClientSecret (DB.clientCredentialsClientSecret cc)
    , V0.clientCredentialsCreated = toDate $ DB.clientCredentialsCreated cc
    }

postClientCredentialsGrant :: () -> V0.PostClientCredentialsGrant -> Handler V0.ClientCredentialsGrant
postClientCredentialsGrant () req = do
  -- check if correct client id && secret
  let clientId = toText $ V0.postClientCredentialsGrantClientId req
  let clientSecret = toText $ V0.postClientCredentialsGrantClientSecret req
  maybeCC <- selectFirst [exact DB.ClientCredentialsClientId clientId, exact DB.ClientCredentialsClientSecret clientSecret] []
  case maybeCC of
    Nothing -> V0.api'throw ()
    Just (Entity _ cc) -> do
      now <- liftIO getCurrentTime
      let user = DB.clientCredentialsUser cc
      accessToken <- liftIO $ generateText64 256
      let secondsFromNow :: Num a => a
          secondsFromNow = 3600
      let expiresIn = addUTCTime (secondsToNominalDiffTime secondsFromNow) now
      let dbToken = DB.ClientCredentialsToken now user accessToken expiresIn
      insert_ dbToken
      let grant = V0.ClientCredentialsGrant (V0.AccessToken accessToken) secondsFromNow
      return grant

deleteClientCredentials :: () -> V0.DeleteClientCredentials -> Handler ()
deleteClientCredentials () (V0.DeleteClientCredentials clientId) = do
  maybeCC <- selectFirst [exact DB.ClientCredentialsClientId (toText clientId)] []
  case maybeCC of
    Nothing -> V0.api'throw ()
    Just (Entity key _) -> delete key

secondsToNominalDiffTime :: Integer -> NominalDiffTime
secondsToNominalDiffTime = fromInteger
