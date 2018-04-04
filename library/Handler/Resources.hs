{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Resources where

import Data.FileEmbed (embedFile)
import Import

getAndroidChrome192R :: Handler TypedContent
getAndroidChrome192R = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/png" $ toContent $(embedFile "resources/android-chrome-192x192.png")

getAndroidChrome384R :: Handler TypedContent
getAndroidChrome384R = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/png" $ toContent $(embedFile "resources/android-chrome-384x384.png")

getAppleTouchIconR :: Handler TypedContent
getAppleTouchIconR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/png" $ toContent $(embedFile "resources/apple-touch-icon.png")

getBrowserconfigR :: Handler TypedContent
getBrowserconfigR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/xml" $ toContent $(embedFile "resources/browserconfig.xml")

getFavicon16R :: Handler TypedContent
getFavicon16R = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/png" $ toContent $(embedFile "resources/favicon-16x16.png")

getFavicon32R :: Handler TypedContent
getFavicon32R = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/png" $ toContent $(embedFile "resources/favicon-32x32.png")

getFaviconR :: Handler TypedContent
getFaviconR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/x-icon" $ toContent $(embedFile "resources/favicon.ico")

getMstile150R :: Handler TypedContent
getMstile150R = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/png" $ toContent $(embedFile "resources/mstile-150x150.png")

getSafariPinnedTabR :: Handler TypedContent
getSafariPinnedTabR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "image/svg+xml" $ toContent $(embedFile "resources/safari-pinned-tab.svg")

getSiteWebmanifestR :: Handler TypedContent
getSiteWebmanifestR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  return $ TypedContent "text/cache-manifest" $ toContent $(embedFile "resources/site.webmanifest")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain $ toContent $(embedFile "resources/robots.txt")
