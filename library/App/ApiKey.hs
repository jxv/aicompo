module App.ApiKey where

import Data.Time
import Control.Monad.Persist

import qualified DB
import Import (Handler, Text, liftIO)
import Util

generateAppPublic :: IO Text
generateAppPublic = generateText64 48

generateApiKey :: IO Text
generateApiKey = generateText64 80

insertApp :: Text -> Text -> Handler ()
insertApp userPublic appName = do
  now <- liftIO getCurrentTime
  appPublic <- liftIO generateAppPublic
  insert_ (DB.App now appPublic appName)
  insert_ (DB.UserXApp now userPublic appPublic)

insertApiKey :: Text -> Handler ()
insertApiKey appPublic = do
  now <- liftIO getCurrentTime
  apiKey <- liftIO generateApiKey
  insert_ (DB.ApiKey now appPublic apiKey)

secondsToNominalDiffTime :: Integer -> NominalDiffTime
secondsToNominalDiffTime = fromInteger
