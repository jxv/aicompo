module AiCompo.Bot where

import Data.Time
import Control.Monad.Persist

import qualified DB
import Import (Handler, Text, liftIO)
import Util

generateBotPublic :: IO Text
generateBotPublic = generateText64 48

generateApiKey :: IO Text
generateApiKey = generateText64 80

insertBot :: Text -> Text -> Handler ()
insertBot userPublic botName = do
  now <- liftIO getCurrentTime
  botPublic <- liftIO generateBotPublic
  insert_ (DB.Bot now botPublic botName)
  insert_ (DB.UserXBot now userPublic botPublic)

insertApiKey :: Text -> Handler ()
insertApiKey botPublic = do
  now <- liftIO getCurrentTime
  apiKey <- liftIO generateApiKey
  insert_ (DB.ApiKey now botPublic apiKey)

secondsToNominalDiffTime :: Integer -> NominalDiffTime
secondsToNominalDiffTime = fromInteger
