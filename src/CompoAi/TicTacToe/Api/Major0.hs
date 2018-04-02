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
module CompoAi.TicTacToe.Api.Major0
  ( ticTacToe'version
  , ticTacToe'pull
  , ticTacToe'handler
  , ticTacToe'spec
  , TicTacToe'Thrower(..)
  , TicTacToe'Service(..)
  , AccessToken(..)
  , UserId(..)
  , Group(..)
  , GameId(..)
  , GameToken(..)
  , Board(..)
  , Loc(..)
  , State(..)
  , Users(..)
  , Init(..)
  , PostMove(..)
  , Player(..)
  , Error(..)
  , Final(..)
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
  ticTacToe'PostStart :: meta -> m Init
  ticTacToe'PostMove :: meta -> PostMove -> m State

instance TicTacToe'Service meta m => TicTacToe'Service meta (M.ExceptT C.Response m) where
  ticTacToe'PostStart _meta = M.lift P.$ ticTacToe'PostStart _meta
  ticTacToe'PostMove _meta = M.lift P.. ticTacToe'PostMove _meta

--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Wrap: AccessToken
newtype AccessToken = AccessToken R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText, P.Show)

-- Wrap: UserId
newtype UserId = UserId R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText, P.Show)

-- Wrap: Group
newtype Group = Group R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText, P.Show)

-- Wrap: GameId
newtype GameId = GameId R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText, P.Show)

-- Wrap: GameToken
newtype GameToken = GameToken R.Text
  deriving (P.Eq, P.Ord, P.IsString, R.ToText, P.Show)

-- Struct: Board
data Board = Board
  { boardCells :: [[(P.Maybe Player)]]
  } deriving (P.Show, P.Eq)

-- Struct: Loc
data Loc = Loc
  { locx :: P.Int
  , locy :: P.Int
  } deriving (P.Show, P.Eq)

-- Struct: State
data State = State
  { stateBoard :: Board
  , stateFinal :: (P.Maybe Final)
  } deriving (P.Show, P.Eq)

-- Struct: Users
data Users = Users
  { usersx :: UserId
  , userso :: UserId
  } deriving (P.Show, P.Eq)

-- Struct: Init
data Init = Init
  { initGameId :: GameId
  , initUsers :: Users
  , initState :: State
  } deriving (P.Show, P.Eq)

-- Struct: PostMove
data PostMove = PostMove
  { postMoveLoc :: Loc
  , postMoveGameId :: GameId
  } deriving (P.Show, P.Eq)

-- Enumeration: Player
data Player
  = Player'X
  | Player'O
  deriving (P.Show, P.Eq)

-- Enumeration: Error
data Error
  = Error'AccessToken
  | Error'GameId
  | Error'Unauthorized
  | Error'MoveLoc
  | Error'Timeout
  deriving (P.Show, P.Eq)

-- Enumeration: Final
data Final
  = Final'Won
  | Final'Loss
  | Final'Tied
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Add-ons
--------------------------------------------------------

ticTacToe'Scotty'Post
  :: (Scotty.ScottyError e, R.MonadIO m, TicTacToe'Service meta m, R.MonadCatch m)
  => ([(Scotty.LazyText, Scotty.LazyText)] -> C.Hooks m AccessToken meta)
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
  => (xtra -> C.Hooks m AccessToken meta)
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
    TicTacToe'Api'PostStart -> C.toVal P.<$> ticTacToe'PostStart meta'
    TicTacToe'Api'PostMove a' -> C.toVal P.<$> ticTacToe'PostMove meta' a'

-- API Parser
ticTacToe'ApiParser :: C.ApiParser TicTacToe'Api
ticTacToe'ApiParser = C.ApiParser
  { C.hollow = R.fromList
     [ ("PostStart", TicTacToe'Api'PostStart)
     ]
  , C.struct = R.fromList
     [ ("PostMove", v TicTacToe'Api'PostMove)
     ]
  , C.enumeration = R.empty
  , C.wrap = R.empty
  }
  where
    v x y = x P.<$> C.fromVal y

-- Api
data TicTacToe'Api
  = TicTacToe'Api'PostStart
  | TicTacToe'Api'PostMove PostMove
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

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

instance C.ToVal Group where
  toVal (Group _w) = C.toVal _w

instance C.FromVal Group where
  fromVal _v = Group P.<$> C.fromVal _v

instance R.ToJSON Group where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Group where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal GameId where
  toVal (GameId _w) = C.toVal _w

instance C.FromVal GameId where
  fromVal _v = GameId P.<$> C.fromVal _v

instance R.ToJSON GameId where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON GameId where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal GameToken where
  toVal (GameToken _w) = C.toVal _w

instance C.FromVal GameToken where
  fromVal _v = GameToken P.<$> C.fromVal _v

instance R.ToJSON GameToken where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON GameToken where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Board where
  toVal Board
    { boardCells
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("cells", C.toVal boardCells)
    ]

instance C.FromVal Board where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Board
      P.<$> C.getMember _m "cells"
    _ -> P.Nothing

instance R.ToJSON Board where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Board where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Loc where
  toVal Loc
    { locx
    , locy
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("x", C.toVal locx)
    , ("y", C.toVal locy)
    ]

instance C.FromVal Loc where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Loc
      P.<$> C.getMember _m "x"
      P.<*> C.getMember _m "y"
    _ -> P.Nothing

instance R.ToJSON Loc where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Loc where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal State where
  toVal State
    { stateBoard
    , stateFinal
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("board", C.toVal stateBoard)
    , ("final", C.toVal stateFinal)
    ]

instance C.FromVal State where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> State
      P.<$> C.getMember _m "board"
      P.<*> C.getMember _m "final"
    _ -> P.Nothing

instance R.ToJSON State where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON State where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Users where
  toVal Users
    { usersx
    , userso
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("x", C.toVal usersx)
    , ("o", C.toVal userso)
    ]

instance C.FromVal Users where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Users
      P.<$> C.getMember _m "x"
      P.<*> C.getMember _m "o"
    _ -> P.Nothing

instance R.ToJSON Users where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Users where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Init where
  toVal Init
    { initGameId
    , initUsers
    , initState
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("gameId", C.toVal initGameId)
    , ("users", C.toVal initUsers)
    , ("state", C.toVal initState)
    ]

instance C.FromVal Init where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Init
      P.<$> C.getMember _m "gameId"
      P.<*> C.getMember _m "users"
      P.<*> C.getMember _m "state"
    _ -> P.Nothing

instance R.ToJSON Init where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Init where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal PostMove where
  toVal PostMove
    { postMoveLoc
    , postMoveGameId
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("loc", C.toVal postMoveLoc)
    , ("gameId", C.toVal postMoveGameId)
    ]

instance C.FromVal PostMove where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> PostMove
      P.<$> C.getMember _m "loc"
      P.<*> C.getMember _m "gameId"
    _ -> P.Nothing

instance R.ToJSON PostMove where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON PostMove where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Player where
  toVal = \case
    Player'X -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "X" P.Nothing
    Player'O -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "O" P.Nothing

instance C.FromVal Player where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
      ("X", P.Nothing) -> P.Just Player'X
      ("O", P.Nothing) -> P.Just Player'O
      _ -> P.Nothing
    _ -> P.Nothing

instance R.ToJSON Player where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Player where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Error where
  toVal = \case
    Error'AccessToken -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "AccessToken" P.Nothing
    Error'GameId -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "GameId" P.Nothing
    Error'Unauthorized -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Unauthorized" P.Nothing
    Error'MoveLoc -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "MoveLoc" P.Nothing
    Error'Timeout -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Timeout" P.Nothing

instance C.FromVal Error where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
      ("AccessToken", P.Nothing) -> P.Just Error'AccessToken
      ("GameId", P.Nothing) -> P.Just Error'GameId
      ("Unauthorized", P.Nothing) -> P.Just Error'Unauthorized
      ("MoveLoc", P.Nothing) -> P.Just Error'MoveLoc
      ("Timeout", P.Nothing) -> P.Just Error'Timeout
      _ -> P.Nothing
    _ -> P.Nothing

instance R.ToJSON Error where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Error where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Final where
  toVal = \case
    Final'Won -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Won" P.Nothing
    Final'Loss -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Loss" P.Nothing
    Final'Tied -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Tied" P.Nothing

instance C.FromVal Final where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
      ("Won", P.Nothing) -> P.Just Final'Won
      ("Loss", P.Nothing) -> P.Just Final'Loss
      ("Tied", P.Nothing) -> P.Just Final'Tied
      _ -> P.Nothing
    _ -> P.Nothing

instance R.ToJSON Final where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Final where
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
  where P.Just v = R.decode "{\"fluid\":{\"major\":0,\"minor\":0},\"pull\":{\"protocol\":\"http\",\"name\":\"TicTacToe\",\"host\":\"compo-ai.herokuapp.com\",\"port\":80,\"path\":\"/api/tictactoe\",\"meta\":\"AccessToken\",\"error\":\"Error\"},\"schema\":{\"AccessToken\":\"String\",\"UserId\":\"String\",\"Group\":\"String\",\"GameId\":\"String\",\"GameToken\":\"String\",\"Player\":[\"X\",\"O\"],\"Error\":[\"AccessToken\",\"GameId\",\"Unauthorized\",\"MoveLoc\",\"Timeout\"],\"Board\":{\"m\":[{\"cells\":{\"n\":\"List\",\"p\":{\"n\":\"List\",\"p\":{\"n\":\"Option\",\"p\":\"Player\"}}}}]},\"Final\":[\"Won\",\"Loss\",\"Tied\"],\"Loc\":{\"m\":[{\"x\":\"Int\"},{\"y\":\"Int\"}]},\"State\":{\"m\":[{\"board\":\"Board\"},{\"final\":{\"n\":\"Option\",\"p\":\"Final\"}}]},\"Users\":{\"m\":[{\"x\":\"UserId\"},{\"o\":\"UserId\"}]},\"Init\":{\"m\":[{\"gameId\":\"GameId\"},{\"users\":\"Users\"},{\"state\":\"State\"}]},\"PostStart\":{\"o\":\"Init\"},\"PostMove\":{\"m\":[{\"loc\":\"Loc\"},{\"gameId\":\"GameId\"}],\"o\":\"State\"}},\"version\":{\"major\":0,\"minor\":0}}"
