module HandlerUtil where

import Control.Monad.Trans.Maybe
import Yesod.Auth

import qualified DB
import Import

getAuthUserPublic :: Handler (Maybe Text)
getAuthUserPublic = runMaybeT $ do
  (_,val) <- MaybeT maybeAuthPair
  pure $ DB.userPublic val
