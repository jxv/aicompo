-- Pragmas
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module
module AiCompo.Authentication.Api.Major0
  ( api'version
  , api'pull
  , api'handler
  , api'spec
  , Api'Thrower(..)
  , Api'Service(..)
  , Seconds(..)
  , UserId(..)
  , Email(..)
  , AccessToken(..)
  , ClientId(..)
  , ClientSecret(..)
  , Date(..)
  , User(..)
  , GetUser(..)
  , ClientCredentials(..)
  , ClientCredentialsGrant(..)
  , GetClientCredentials(..)
  , PostClientCredentialsGrant(..)
  , DeleteClientCredentials(..)
  , Role(..)
  ) where

-- Imports
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Control.Monad.Except as M
import qualified Data.IORef as IO
import qualified Data.String as P (IsString)
import qualified Fluid.Imports as R
import qualified Fluid.Server as C

--------------------------------------------------------
-- Configs
--------------------------------------------------------

-- Version
api'version :: C.Version
api'version = C.Version 0 0

api'pull :: C.Pull
api'pull = C.Pull "http" "aicompo.herokuapp.com" "/api" 80

--------------------------------------------------------
-- Interfaces
--------------------------------------------------------

-- Thrower
class C.ServiceThrower m => Api'Thrower m where
  api'throw :: () -> m a
  api'throw = C.serviceThrow P.. R.toJSON P.. C.toVal

-- Service
class P.Monad m => Api'Service meta m where
  api'GetUserCount :: meta -> m P.Int
  api'GetUser :: meta -> GetUser -> m User
  api'GetClientCredentials :: meta -> GetClientCredentials -> m [ClientCredentials]
  api'PostClientCredentialsGrant :: meta -> PostClientCredentialsGrant -> m ClientCredentialsGrant
  api'DeleteClientCredentials :: meta -> DeleteClientCredentials -> m ()

instance Api'Service meta m => Api'Service meta (M.ExceptT C.Response m) where
  api'GetUserCount _meta = M.lift P.$ api'GetUserCount _meta
  api'GetUser _meta = M.lift P.. api'GetUser _meta
  api'GetClientCredentials _meta = M.lift P.. api'GetClientCredentials _meta
  api'PostClientCredentialsGrant _meta = M.lift P.. api'PostClientCredentialsGrant _meta
  api'DeleteClientCredentials _meta = M.lift P.. api'DeleteClientCredentials _meta

--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Wrap: Seconds
newtype Seconds = Seconds P.Int
  deriving (P.Eq, P.Ord, P.Num, P.Enum, P.Show)

-- Wrap: UserId
newtype UserId = UserId R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText, P.Show)

-- Wrap: Email
newtype Email = Email R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText, P.Show)

-- Wrap: AccessToken
newtype AccessToken = AccessToken R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText, P.Show)

-- Wrap: ClientId
newtype ClientId = ClientId R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText, P.Show)

-- Wrap: ClientSecret
newtype ClientSecret = ClientSecret R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText, P.Show)

-- Struct: Date
data Date = Date
  { dateYear :: P.Int
  , dateMonth :: P.Int
  , dateDay :: P.Int
  } deriving (P.Show, P.Eq)

-- Struct: User
data User = User
  { userUserId :: UserId
  , userEmail :: (P.Maybe Email)
  , userRole :: Role
  } deriving (P.Show, P.Eq)

-- Struct: GetUser
data GetUser = GetUser
  { getUserUserId :: UserId
  } deriving (P.Show, P.Eq)

-- Struct: ClientCredentials
data ClientCredentials = ClientCredentials
  { clientCredentialsClientId :: ClientId
  , clientCredentialsClientSecret :: ClientSecret
  , clientCredentialsCreated :: Date
  } deriving (P.Show, P.Eq)

-- Struct: ClientCredentialsGrant
data ClientCredentialsGrant = ClientCredentialsGrant
  { clientCredentialsGrantAccessToken :: AccessToken
  , clientCredentialsGrantExpiresIn :: Seconds
  } deriving (P.Show, P.Eq)

-- Struct: GetClientCredentials
-- | Attain a list of valid client credentials
data GetClientCredentials = GetClientCredentials
  { getClientCredentialsUserId :: UserId
  } deriving (P.Show, P.Eq)

-- Struct: PostClientCredentialsGrant
data PostClientCredentialsGrant = PostClientCredentialsGrant
  { postClientCredentialsGrantClientId :: ClientId
  , postClientCredentialsGrantClientSecret :: ClientSecret
  } deriving (P.Show, P.Eq)

-- Struct: DeleteClientCredentials
data DeleteClientCredentials = DeleteClientCredentials
  { deleteClientCredentialsClientId :: ClientId
  } deriving (P.Show, P.Eq)

-- Enumeration: Role
data Role
  = Role'Admin
  | Role'Member
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Add-ons
--------------------------------------------------------

--------------------------------------------------------
-- Request handling
--------------------------------------------------------

-- Handler
api'handler
  :: (Api'Service meta m, R.MonadIO m, R.MonadCatch m)
  => (xtra -> C.Hooks m () meta)
  -> xtra
  -> C.Request
  -> m (P.Either C.Response C.Response)
api'handler _hooksBuilder xtra C.Request{C.meta=meta, C.query=query} = R.catch
  (M.runExceptT P.$ do
    meta' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableMeta) P.return (C.fromValFromJson meta)
    let _hooks = _hooksBuilder xtra
    xformMeta <- M.lift P.$ C.metaMiddleware _hooks meta'
    envRef <- R.liftIO C.emptyEnv
    variableBaseCount <- R.liftIO (R.size P.<$> IO.readIORef envRef)
    _limits <- M.lift P.$ C.sandboxLimits _hooks xformMeta
    let _limits' = _limits
          { C.variables = P.fmap (P.+ variableBaseCount) (C.variables _limits)
          }
    _serviceCallCountRef <- R.liftIO (IO.newIORef 0)
    _lambdaCountRef <- R.liftIO (IO.newIORef 0)
    _exprCountRef <- R.liftIO (IO.newIORef 0)
    let evalConfig = C.EvalConfig
          { C.limits = _limits'
          , C.langServiceCallCount = _serviceCallCountRef
          , C.langLambdaCount = _lambdaCountRef
          , C.langExprCount = _exprCountRef
          , C.apiCall = api'ApiCall xformMeta
          }
    query' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableQuery) P.return (C.jsonToExpr query)
    vals <- C.runEval (C.forceVal P.=<< C.eval query' envRef) evalConfig
    P.return P.$ C.Response'Success (R.toJSON vals) _limits)
  (\(C.ThrownValue _err) -> P.return P.. P.Left P.$ C.Response'Error (C.ResponseError'Service _err))

-- API
api'ApiCall :: (Api'Service meta m, C.ServiceThrower m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val
api'ApiCall meta' apiCall' = case C.parseApiCall api'ApiParser apiCall' of
  P.Nothing -> C.runtimeThrow (C.RuntimeError'UnrecognizedCall P.$ C.apiCallName apiCall')
  P.Just x' -> case x' of
    Api'Api'GetUserCount -> C.toVal P.<$> api'GetUserCount meta'
    Api'Api'GetUser a' -> C.toVal P.<$> api'GetUser meta' a'
    Api'Api'GetClientCredentials a' -> C.toVal P.<$> api'GetClientCredentials meta' a'
    Api'Api'PostClientCredentialsGrant a' -> C.toVal P.<$> api'PostClientCredentialsGrant meta' a'
    Api'Api'DeleteClientCredentials a' -> C.toVal P.<$> api'DeleteClientCredentials meta' a'

-- API Parser
api'ApiParser :: C.ApiParser Api'Api
api'ApiParser = C.ApiParser
  { C.hollow = R.fromList
     [ ("GetUserCount", Api'Api'GetUserCount)
     ]
  , C.struct = R.fromList
     [ ("GetUser", v Api'Api'GetUser)
     , ("GetClientCredentials", v Api'Api'GetClientCredentials)
     , ("PostClientCredentialsGrant", v Api'Api'PostClientCredentialsGrant)
     , ("DeleteClientCredentials", v Api'Api'DeleteClientCredentials)
     ]
  , C.enumeration = R.empty
  , C.wrap = R.empty
  }
  where
    v x y = x P.<$> C.fromVal y

-- Api
data Api'Api
  = Api'Api'GetUserCount
  | Api'Api'GetUser GetUser
  | Api'Api'GetClientCredentials GetClientCredentials
  | Api'Api'PostClientCredentialsGrant PostClientCredentialsGrant
  | Api'Api'DeleteClientCredentials DeleteClientCredentials
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

instance C.ToVal Seconds where
  toVal (Seconds _w) = C.toVal _w

instance C.FromVal Seconds where
  fromVal _v = Seconds P.<$> C.fromVal _v

instance R.ToJSON Seconds where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Seconds where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal UserId where
  toVal (UserId _w) = C.toVal _w

instance C.FromVal UserId where
  fromVal _v = UserId P.<$> C.fromVal _v

instance R.ToJSON UserId where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON UserId where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Email where
  toVal (Email _w) = C.toVal _w

instance C.FromVal Email where
  fromVal _v = Email P.<$> C.fromVal _v

instance R.ToJSON Email where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Email where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal AccessToken where
  toVal (AccessToken _w) = C.toVal _w

instance C.FromVal AccessToken where
  fromVal _v = AccessToken P.<$> C.fromVal _v

instance R.ToJSON AccessToken where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON AccessToken where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal ClientId where
  toVal (ClientId _w) = C.toVal _w

instance C.FromVal ClientId where
  fromVal _v = ClientId P.<$> C.fromVal _v

instance R.ToJSON ClientId where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON ClientId where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal ClientSecret where
  toVal (ClientSecret _w) = C.toVal _w

instance C.FromVal ClientSecret where
  fromVal _v = ClientSecret P.<$> C.fromVal _v

instance R.ToJSON ClientSecret where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON ClientSecret where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Date where
  toVal Date
    { dateYear
    , dateMonth
    , dateDay
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("year", C.toVal dateYear)
    , ("month", C.toVal dateMonth)
    , ("day", C.toVal dateDay)
    ]

instance C.FromVal Date where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Date
      P.<$> C.getMember _m "year"
      P.<*> C.getMember _m "month"
      P.<*> C.getMember _m "day"
    _ -> P.Nothing

instance R.ToJSON Date where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Date where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal User where
  toVal User
    { userUserId
    , userEmail
    , userRole
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("userId", C.toVal userUserId)
    , ("email", C.toVal userEmail)
    , ("role", C.toVal userRole)
    ]

instance C.FromVal User where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> User
      P.<$> C.getMember _m "userId"
      P.<*> C.getMember _m "email"
      P.<*> C.getMember _m "role"
    _ -> P.Nothing

instance R.ToJSON User where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON User where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal GetUser where
  toVal GetUser
    { getUserUserId
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("userId", C.toVal getUserUserId)
    ]

instance C.FromVal GetUser where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> GetUser
      P.<$> C.getMember _m "userId"
    _ -> P.Nothing

instance R.ToJSON GetUser where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON GetUser where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal ClientCredentials where
  toVal ClientCredentials
    { clientCredentialsClientId
    , clientCredentialsClientSecret
    , clientCredentialsCreated
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("clientId", C.toVal clientCredentialsClientId)
    , ("clientSecret", C.toVal clientCredentialsClientSecret)
    , ("created", C.toVal clientCredentialsCreated)
    ]

instance C.FromVal ClientCredentials where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> ClientCredentials
      P.<$> C.getMember _m "clientId"
      P.<*> C.getMember _m "clientSecret"
      P.<*> C.getMember _m "created"
    _ -> P.Nothing

instance R.ToJSON ClientCredentials where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON ClientCredentials where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal ClientCredentialsGrant where
  toVal ClientCredentialsGrant
    { clientCredentialsGrantAccessToken
    , clientCredentialsGrantExpiresIn
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("accessToken", C.toVal clientCredentialsGrantAccessToken)
    , ("expiresIn", C.toVal clientCredentialsGrantExpiresIn)
    ]

instance C.FromVal ClientCredentialsGrant where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> ClientCredentialsGrant
      P.<$> C.getMember _m "accessToken"
      P.<*> C.getMember _m "expiresIn"
    _ -> P.Nothing

instance R.ToJSON ClientCredentialsGrant where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON ClientCredentialsGrant where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal GetClientCredentials where
  toVal GetClientCredentials
    { getClientCredentialsUserId
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("userId", C.toVal getClientCredentialsUserId)
    ]

instance C.FromVal GetClientCredentials where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> GetClientCredentials
      P.<$> C.getMember _m "userId"
    _ -> P.Nothing

instance R.ToJSON GetClientCredentials where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON GetClientCredentials where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal PostClientCredentialsGrant where
  toVal PostClientCredentialsGrant
    { postClientCredentialsGrantClientId
    , postClientCredentialsGrantClientSecret
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("clientId", C.toVal postClientCredentialsGrantClientId)
    , ("clientSecret", C.toVal postClientCredentialsGrantClientSecret)
    ]

instance C.FromVal PostClientCredentialsGrant where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> PostClientCredentialsGrant
      P.<$> C.getMember _m "clientId"
      P.<*> C.getMember _m "clientSecret"
    _ -> P.Nothing

instance R.ToJSON PostClientCredentialsGrant where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON PostClientCredentialsGrant where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal DeleteClientCredentials where
  toVal DeleteClientCredentials
    { deleteClientCredentialsClientId
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("clientId", C.toVal deleteClientCredentialsClientId)
    ]

instance C.FromVal DeleteClientCredentials where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> DeleteClientCredentials
      P.<$> C.getMember _m "clientId"
    _ -> P.Nothing

instance R.ToJSON DeleteClientCredentials where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON DeleteClientCredentials where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Role where
  toVal = \case
    Role'Admin -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Admin" P.Nothing
    Role'Member -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Member" P.Nothing

instance C.FromVal Role where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
      ("Admin", P.Nothing) -> P.Just Role'Admin
      ("Member", P.Nothing) -> P.Just Role'Member
      _ -> P.Nothing
    _ -> P.Nothing

instance R.ToJSON Role where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Role where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

--------------------------------------------------------
-- Spec
--------------------------------------------------------

api'spec :: R.Value
api'spec = v
  where P.Just v = R.decode "{\"fluid\":{\"major\":0,\"minor\":0},\"pull\":{\"protocol\":\"http\",\"name\":\"Api\",\"host\":\"aicompo.herokuapp.com\",\"meta\":\"Unit\",\"path\":\"/api\",\"port\":80,\"error\":\"Unit\"},\"schema\":{\"Date\":{\"m\":[{\"year\":\"Int\"},{\"month\":\"Int\"},{\"day\":\"Int\"}]},\"Seconds\":\"Int\",\"UserId\":\"String\",\"Email\":\"String\",\"Role\":[\"Admin\",\"Member\"],\"User\":{\"m\":[{\"userId\":\"UserId\"},{\"email\":{\"n\":\"Option\",\"p\":\"Email\"}},{\"role\":\"Role\"}]},\"GetUser\":{\"m\":[{\"userId\":\"UserId\"}],\"o\":\"User\"},\"GetUserCount\":{\"o\":\"Int\"},\"AccessToken\":\"String\",\"ClientId\":\"String\",\"ClientSecret\":\"String\",\"ClientCredentials\":{\"m\":[{\"clientId\":\"ClientId\"},{\"clientSecret\":\"ClientSecret\"},{\"created\":\"Date\"}]},\"ClientCredentialsGrant\":{\"m\":[{\"accessToken\":\"AccessToken\"},{\"expiresIn\":\"Seconds\"}]},\"GetClientCredentials\":{\"m\":[{\"userId\":\"UserId\"}],\"o\":{\"n\":\"List\",\"p\":\"ClientCredentials\"},\"c\":\"Attain a list of valid client credentials\"},\"PostClientCredentialsGrant\":{\"m\":[{\"clientId\":\"ClientId\"},{\"clientSecret\":\"ClientSecret\"}],\"o\":\"ClientCredentialsGrant\"},\"DeleteClientCredentials\":{\"m\":[{\"clientId\":\"ClientId\"}],\"o\":\"Unit\"}},\"version\":{\"major\":0,\"minor\":0}}"
