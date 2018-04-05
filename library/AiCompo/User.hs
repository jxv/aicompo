{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AiCompo.User
  () where
{-
import Protolude
import Data.Text.Conversions
import Control.Monad.Persist

import qualified DB as DB
import qualified AiCompo.Authentication.Api.Major0 as V0
import Import (Entity(..), Filter(..))
-- import AiCompo.Authentication.Api.Monad ()
import Util

genUserPublic :: IO Text
genUserPublic = generateText64 16

getUser :: (MonadPersist SqlBackend m, V0.Api'Thrower m) => () -> V0.GetUser -> m V0.User
getUser () req = do
  maybeEntity <- getBy $ DB.UniqueUserPublic (toText $ V0.getUserUserId req)
  case maybeEntity of
    Nothing -> V0.api'throw ()
    Just Entity{entityVal=user} -> do
      return $ V0.User
        { V0.userUserId = V0.UserId $ DB.userPublic user
        , V0.userEmail = V0.Email <$> (DB.userEmail user)
        , V0.userRole = apiRole (DB.userRole user)
        }

apiRole :: DB.Role -> V0.Role
apiRole = \case
  DB.Role'Admin -> V0.Role'Admin
  DB.Role'Member -> V0.Role'Member

getUserCount :: MonadPersist SqlBackend m => () -> m Int
getUserCount () = count ([] :: [Filter DB.User]

-}
