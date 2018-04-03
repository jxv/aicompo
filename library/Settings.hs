{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Settings where

import ClassyPrelude.Yesod
import qualified Control.Exception as Exception
import Data.Aeson (Result (..), fromJSON, withObject, (.!=), (.:?))
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax (Exp, Name, Q)
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)
import Yesod.Default.Util (WidgetFileSettings, widgetFileNoReload, widgetFileReload)

data AppSettings = AppSettings
  { appStaticDir :: String
  , appDatabaseConf :: PostgresConf
  , appRoot :: Maybe Text
  , appHost :: HostPreference
  , appPort :: Int
  , appIpFromHeader :: Bool
  , appDetailedRequestLogging :: Bool
  , appShouldLogAll :: Bool
  , appReloadTemplates :: Bool
  , appMutableStatic :: Bool
  , appSkipCombining :: Bool
  , appCopyright :: Text
  , appAnalytics :: Maybe Text
  , appAuthDummyLogin :: Bool
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" $ \o -> do
    let defaultDev =
#ifdef DEVELOPMENT
          True
#else
          False
#endif
    appStaticDir <- o .: "static-dir"
    appDatabaseConf <- o .: "database"
    appRoot <- o .:? "approot"
    appHost <- fromString <$> o .: "host"
    appPort <- o .: "port"
    appIpFromHeader <- o .: "ip-from-header"
    appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
    appShouldLogAll <- o .:? "should-log-all" .!= defaultDev
    appReloadTemplates <- o .:? "reload-templates" .!= defaultDev
    appMutableStatic <- o .:? "mutable-static" .!= defaultDev
    appSkipCombining <- o .:? "skip-combining" .!= defaultDev
    appCopyright <- o .: "copyright"
    appAnalytics <- o .:? "analytics"
    appAuthDummyLogin <- o .:? "auth-dummy-login" .!= defaultDev
    return AppSettings {..}

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

combineSettings :: CombineSettings
combineSettings = def

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
