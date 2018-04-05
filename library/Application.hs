{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
( getApplicationDev
, appMain
, develMain
, makeFoundation
, makeLogWare
-- * for DevelMain
, getApplicationRepl
, shutdownApp
-- * for GHCI
, handler
, db
) where

import qualified Data.Text as T
import Control.Monad.Logger (liftLoc, runLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings, defaultShouldDisplayException, runSettings, setHost, setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger), IPAddrSource (..), OutputFormat (..), destination, mkRequestLogger, outputFormat)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
import System.Environment (getEnv)

import Yesod.Auth
import Yesod.Auth.OAuth2.Github

import qualified AiCompo.TicTacToe.Service as TS
import Import
import Handler.Resources
import Handler.Root
import Handler.Developer
import Handler.ApiKey
import Handler.ApiKeyEntity
import Handler.Spa
import Handler.ApiTicTacToe
import DB (migrate)

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  appHttpManager <- newManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appAuthPlugins <- sequence
    [ loadPlugin oauth2Github "GITHUB"
    ]
  appStatic <-
    (if appMutableStatic appSettings then staticDevel else static)
    (appStaticDir appSettings)
  appTicTacToeComponents <- TS.newComponents
  _ <- TS.forkDispatcher appTicTacToeComponents
  let mkFoundation appConnPool = App {..} -- chiken and the egg problem
  let tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
  let logFunc = messageLoggerSource tempFoundation appLogger
  pool <- flip runLoggingT logFunc $ createPostgresqlPool
    (pgConnStr  $ appDatabaseConf appSettings)
    (pgPoolSize $ appDatabaseConf appSettings)
  migrate (pgConnStr $ appDatabaseConf appSettings)
  return $ mkFoundation pool
  where
    loadPlugin f prefix = do
        clientId <- getEnv $ prefix <> "_CLIENT_ID"
        clientSecret <- getEnv $ prefix <> "_CLIENT_SECRET"
        pure $ f (T.pack clientId) (T.pack clientSecret)


makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  appPlain <- toWaiAppPlain foundation
  return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger def
      { outputFormat =
          if appDetailedRequestLogging $ appSettings foundation
            then Detailed True
            else Apache
              (if appIpFromHeader $ appSettings foundation
                then FromFallback
                else FromSocket)
      , destination = Logger $ loggerSet $ appLogger foundation
      }

warpSettings :: App -> Settings
warpSettings foundation =
    setPort (appPort $ appSettings foundation)
  $ setHost (appHost $ appSettings foundation)
  $ setOnException (\_req e ->
      when (defaultShouldDisplayException e) $ messageLoggerSource
        foundation
        (appLogger foundation)
        $(qLocation >>= liftLoc)
        "yesod"
        LevelError
        (toLogStr $ "Exception from Warp: " ++ show e))
    defaultSettings

getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

develMain :: IO ()
develMain = develMainHelper getApplicationDev

appMain :: IO ()
appMain = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  foundation <- makeFoundation settings
  app <- makeApplication foundation
  runSettings (warpSettings foundation) app


-- For DevelMain.hs as a way to run the app from GHCi
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app1 <- makeApplication foundation
  return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()

-- For GHCi
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- For GHCi
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB
