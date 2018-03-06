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
module CompoAi.Api.TicTacToe.Major0
  ( ticTacToe'version
  , ticTacToe'pull
  , ticTacToe'handler
  , ticTacToe'spec
  , TicTacToe'Thrower(..)
  , TicTacToe'Service(..)
  , Meta(..)
  , Error(..)
  , Hello(..)
  , ticTacToe'Scotty'Post
  , ticTacToe'Scotty'Get
  ) where

-- Imports
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Control.Monad.Except as M
import qualified Data.IORef as IO
import qualified Data.String as P (IsString)
import qualified Fluid.Imports as R
import qualified Fluid.Server as C
import qualified Fluid.Server.Scotty as Scotty

--------------------------------------------------------
-- Configs
--------------------------------------------------------

-- Version
ticTacToe'version :: C.Version
ticTacToe'version = C.Version 0 0

ticTacToe'pull :: C.Pull
ticTacToe'pull = C.Pull "http" "compo-ai.herokuapp.com" "/api/tictactoe" 80

--------------------------------------------------------
-- Interfaces
--------------------------------------------------------

-- Thrower
class C.ServiceThrower m => TicTacToe'Thrower m where
  ticTacToe'throw :: Error -> m a
  ticTacToe'throw = C.serviceThrow P.. R.toJSON P.. C.toVal

-- Service
class P.Monad m => TicTacToe'Service meta m where
  ticTacToe'Hello :: meta -> Hello -> m R.Text

instance TicTacToe'Service meta m => TicTacToe'Service meta (M.ExceptT C.Response m) where
  ticTacToe'Hello _meta = M.lift P.. ticTacToe'Hello _meta

--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Wrap: Meta
newtype Meta = Meta ()
  deriving (P.Eq, P.Ord, P.Show)

-- Wrap: Error
newtype Error = Error ()
  deriving (P.Eq, P.Ord, P.Show)

-- Struct: Hello
data Hello = Hello
  { helloTarget :: R.Text
  } deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Add-ons
--------------------------------------------------------

ticTacToe'Scotty'Post
  :: (Scotty.ScottyError e, R.MonadIO m, TicTacToe'Service meta m, R.MonadCatch m)
  => ([(Scotty.LazyText, Scotty.LazyText)] -> C.Hooks m Meta meta)
  -> C.Pull
  -> Scotty.ScottyT e m ()
ticTacToe'Scotty'Post _hooks _pull = Scotty.respondSingleton _pull ticTacToe'version (\_xtra -> ticTacToe'handler _hooks _xtra)

ticTacToe'Scotty'Get :: (Scotty.ScottyError e, R.MonadIO m) => C.Pull -> Scotty.ScottyT e m ()
ticTacToe'Scotty'Get = Scotty.getSpec P.$ R.toJSON [ticTacToe'spec]

--------------------------------------------------------
-- Request handling
--------------------------------------------------------

-- Handler
ticTacToe'handler
  :: (TicTacToe'Service meta m, R.MonadIO m, R.MonadCatch m)
  => (xtra -> C.Hooks m Meta meta)
  -> xtra
  -> C.Request
  -> m (P.Either C.Response C.Response)
ticTacToe'handler _hooksBuilder xtra C.Request{C.meta=meta, C.query=query} = R.catch
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
          , C.apiCall = ticTacToe'ApiCall xformMeta
          }
    query' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableQuery) P.return (C.jsonToExpr query)
    vals <- C.runEval (C.forceVal P.=<< C.eval query' envRef) evalConfig
    P.return P.$ C.Response'Success (R.toJSON vals) _limits)
  (\(C.ThrownValue _err) -> P.return P.. P.Left P.$ C.Response'Error (C.ResponseError'Service _err))

-- API
ticTacToe'ApiCall :: (TicTacToe'Service meta m, C.ServiceThrower m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val
ticTacToe'ApiCall meta' apiCall' = case C.parseApiCall ticTacToe'ApiParser apiCall' of
  P.Nothing -> C.runtimeThrow (C.RuntimeError'UnrecognizedCall P.$ C.apiCallName apiCall')
  P.Just x' -> case x' of
    TicTacToe'Api'Hello a' -> C.toVal P.<$> ticTacToe'Hello meta' a'

-- API Parser
ticTacToe'ApiParser :: C.ApiParser TicTacToe'Api
ticTacToe'ApiParser = C.ApiParser
  { C.hollow = R.empty
  , C.struct = R.fromList
     [ ("Hello", v TicTacToe'Api'Hello)
     ]
  , C.enumeration = R.empty
  , C.wrap = R.empty
  }
  where
    v x y = x P.<$> C.fromVal y

-- Api
data TicTacToe'Api
  = TicTacToe'Api'Hello Hello
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

instance C.ToVal Meta where
  toVal (Meta _w) = C.toVal _w

instance C.FromVal Meta where
  fromVal _v = Meta P.<$> C.fromVal _v

instance R.ToJSON Meta where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Meta where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Error where
  toVal (Error _w) = C.toVal _w

instance C.FromVal Error where
  fromVal _v = Error P.<$> C.fromVal _v

instance R.ToJSON Error where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Error where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Hello where
  toVal Hello
    { helloTarget
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("target", C.toVal helloTarget)
    ]

instance C.FromVal Hello where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Hello
      P.<$> C.getMember _m "target"
    _ -> P.Nothing

instance R.ToJSON Hello where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Hello where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

--------------------------------------------------------
-- Spec
--------------------------------------------------------

ticTacToe'spec :: R.Value
ticTacToe'spec = v
  where P.Just v = R.decode "{\"fluid\":{\"major\":0,\"minor\":0},\"pull\":{\"protocol\":\"http\",\"name\":\"TicTacToe\",\"host\":\"compo-ai.herokuapp.com\",\"path\":\"/api/tictactoe\",\"meta\":\"Meta\",\"port\":80,\"error\":\"Error\"},\"schema\":{\"Meta\":\"Unit\",\"Error\":\"Unit\",\"Hello\":{\"m\":[{\"target\":\"String\"}],\"o\":\"String\"}},\"version\":{\"major\":0,\"minor\":0}}"
