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
module AiCompo.TicTacToe.Api.Major0
  ( ticTacToe'version
  , ticTacToe'pull
  , ticTacToe'handler
  , ticTacToe'spec
  , TicTacToe'Thrower(..)
  , TicTacToe'Service(..)
  , ApiKey(..)
  , BotId(..)
  , Group(..)
  , GameId(..)
  , GameToken(..)
  , Meta(..)
  , Board(..)
  , Loc(..)
  , State(..)
  , Bots(..)
  , Init(..)
  , Frame(..)
  , Playback(..)
  , PostMove(..)
  , GetPlayback(..)
  , Player(..)
  , Error(..)
  , Final(..)
  , Result(..)
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
ticTacToe'version :: C.Version
ticTacToe'version = C.Version 0 0

ticTacToe'pull :: C.Pull
ticTacToe'pull = C.Pull "https" "www.aicompo.net" "/api/tictactoe" 443

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
  ticTacToe'GetPlayback :: meta -> GetPlayback -> m Playback

instance TicTacToe'Service meta m => TicTacToe'Service meta (M.ExceptT C.Response m) where
  ticTacToe'PostStart _meta = M.lift P.$ ticTacToe'PostStart _meta
  ticTacToe'PostMove _meta = M.lift P.. ticTacToe'PostMove _meta
  ticTacToe'GetPlayback _meta = M.lift P.. ticTacToe'GetPlayback _meta

--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Wrap: ApiKey
newtype ApiKey = ApiKey R.Text
  deriving (P.Eq, P.Ord, R.ToMarkup, P.IsString, R.ToText, P.Show)

-- Wrap: BotId
newtype BotId = BotId R.Text
  deriving (P.Eq, P.Ord, R.ToMarkup, P.IsString, R.ToText, P.Show)

-- Wrap: Group
newtype Group = Group R.Text
  deriving (P.Eq, P.Ord, R.ToMarkup, P.IsString, R.ToText, P.Show)

-- Wrap: GameId
newtype GameId = GameId R.Text
  deriving (P.Eq, P.Ord, R.ToMarkup, P.IsString, R.ToText, P.Show)

-- Wrap: GameToken
newtype GameToken = GameToken R.Text
  deriving (P.Eq, P.Ord, R.ToMarkup, P.IsString, R.ToText, P.Show)

-- Struct: Meta
data Meta = Meta
  { metaApiKey :: (P.Maybe ApiKey)
  } deriving (P.Show, P.Eq)

-- Struct: Board
data Board = Board
  { boardCells :: [[(P.Maybe Player)]]
  } deriving (P.Show, P.Eq)

-- Struct: Loc
data Loc = Loc
  { locX :: P.Int
  , locY :: P.Int
  } deriving (P.Show, P.Eq)

-- Struct: State
data State = State
  { stateBoard :: Board
  , stateFinal :: (P.Maybe Final)
  } deriving (P.Show, P.Eq)

-- Struct: Bots
data Bots = Bots
  { botsX :: BotId
  , botsO :: BotId
  } deriving (P.Show, P.Eq)

-- Struct: Init
data Init = Init
  { initGameId :: GameId
  , initUsers :: Bots
  , initState :: State
  } deriving (P.Show, P.Eq)

-- Struct: Frame
data Frame = Frame
  { frameBoard :: Board
  , frameLoc :: Loc
  , framePlayer :: Player
  } deriving (P.Show, P.Eq)

-- Struct: Playback
data Playback = Playback
  { playbackFrames :: [Frame]
  , playbackX :: BotId
  , playbackO :: BotId
  , playbackResult :: Result
  } deriving (P.Show, P.Eq)

-- Struct: PostMove
data PostMove = PostMove
  { postMoveLoc :: Loc
  , postMoveGameId :: GameId
  } deriving (P.Show, P.Eq)

-- Struct: GetPlayback
data GetPlayback = GetPlayback
  { getPlaybackGameId :: GameId
  } deriving (P.Show, P.Eq)

-- Enumeration: Player
data Player
  = Player'X
  | Player'O
  deriving (P.Show, P.Eq)

-- Enumeration: Error
data Error
  = Error'GameId
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

-- Enumeration: Result
data Result
  = Result'Tie
  | Result'WinnerX
  | Result'WinnerO
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Add-ons
--------------------------------------------------------

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
    TicTacToe'Api'PostStart -> C.toVal P.<$> ticTacToe'PostStart meta'
    TicTacToe'Api'PostMove a' -> C.toVal P.<$> ticTacToe'PostMove meta' a'
    TicTacToe'Api'GetPlayback a' -> C.toVal P.<$> ticTacToe'GetPlayback meta' a'

-- API Parser
ticTacToe'ApiParser :: C.ApiParser TicTacToe'Api
ticTacToe'ApiParser = C.ApiParser
  { C.hollow = R.fromList
     [ ("PostStart", TicTacToe'Api'PostStart)
     ]
  , C.struct = R.fromList
     [ ("PostMove", v TicTacToe'Api'PostMove)
     , ("GetPlayback", v TicTacToe'Api'GetPlayback)
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
  | TicTacToe'Api'GetPlayback GetPlayback
  deriving (P.Show, P.Eq)

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

instance C.ToVal ApiKey where
  toVal (ApiKey _w) = C.toVal _w

instance C.FromVal ApiKey where
  fromVal _v = ApiKey P.<$> C.fromVal _v

instance R.ToJSON ApiKey where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON ApiKey where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal BotId where
  toVal (BotId _w) = C.toVal _w

instance C.FromVal BotId where
  fromVal _v = BotId P.<$> C.fromVal _v

instance R.ToJSON BotId where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON BotId where
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

instance C.ToVal Meta where
  toVal Meta
    { metaApiKey
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("apiKey", C.toVal metaApiKey)
    ]

instance C.FromVal Meta where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Meta
      P.<$> C.getMember _m "apiKey"
    _ -> P.Nothing

instance R.ToJSON Meta where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Meta where
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
    { locX
    , locY
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("x", C.toVal locX)
    , ("y", C.toVal locY)
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

instance C.ToVal Bots where
  toVal Bots
    { botsX
    , botsO
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("x", C.toVal botsX)
    , ("o", C.toVal botsO)
    ]

instance C.FromVal Bots where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Bots
      P.<$> C.getMember _m "x"
      P.<*> C.getMember _m "o"
    _ -> P.Nothing

instance R.ToJSON Bots where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Bots where
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

instance C.ToVal Frame where
  toVal Frame
    { frameBoard
    , frameLoc
    , framePlayer
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("board", C.toVal frameBoard)
    , ("loc", C.toVal frameLoc)
    , ("player", C.toVal framePlayer)
    ]

instance C.FromVal Frame where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Frame
      P.<$> C.getMember _m "board"
      P.<*> C.getMember _m "loc"
      P.<*> C.getMember _m "player"
    _ -> P.Nothing

instance R.ToJSON Frame where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Frame where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Playback where
  toVal Playback
    { playbackFrames
    , playbackX
    , playbackO
    , playbackResult
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("frames", C.toVal playbackFrames)
    , ("x", C.toVal playbackX)
    , ("o", C.toVal playbackO)
    , ("result", C.toVal playbackResult)
    ]

instance C.FromVal Playback where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Playback
      P.<$> C.getMember _m "frames"
      P.<*> C.getMember _m "x"
      P.<*> C.getMember _m "o"
      P.<*> C.getMember _m "result"
    _ -> P.Nothing

instance R.ToJSON Playback where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Playback where
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

instance C.ToVal GetPlayback where
  toVal GetPlayback
    { getPlaybackGameId
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("gameId", C.toVal getPlaybackGameId)
    ]

instance C.FromVal GetPlayback where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> GetPlayback
      P.<$> C.getMember _m "gameId"
    _ -> P.Nothing

instance R.ToJSON GetPlayback where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON GetPlayback where
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
    Error'GameId -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "GameId" P.Nothing
    Error'Unauthorized -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Unauthorized" P.Nothing
    Error'MoveLoc -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "MoveLoc" P.Nothing
    Error'Timeout -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Timeout" P.Nothing

instance C.FromVal Error where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
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

instance C.ToVal Result where
  toVal = \case
    Result'Tie -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Tie" P.Nothing
    Result'WinnerX -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "WinnerX" P.Nothing
    Result'WinnerO -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "WinnerO" P.Nothing

instance C.FromVal Result where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
      ("Tie", P.Nothing) -> P.Just Result'Tie
      ("WinnerX", P.Nothing) -> P.Just Result'WinnerX
      ("WinnerO", P.Nothing) -> P.Just Result'WinnerO
      _ -> P.Nothing
    _ -> P.Nothing

instance R.ToJSON Result where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Result where
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
  where P.Just v = R.decode "{\"fluid\":{\"major\":0,\"minor\":0},\"pull\":{\"protocol\":\"https\",\"name\":\"TicTacToe\",\"host\":\"www.aicompo.net\",\"port\":443,\"path\":\"/api/tictactoe\",\"meta\":\"Meta\",\"error\":\"Error\"},\"schema\":{\"Meta\":{\"m\":[{\"apiKey\":{\"n\":\"Option\",\"p\":\"ApiKey\"}}]},\"ApiKey\":\"String\",\"BotId\":\"String\",\"Group\":\"String\",\"GameId\":\"String\",\"GameToken\":\"String\",\"Player\":[\"X\",\"O\"],\"Error\":[\"GameId\",\"Unauthorized\",\"MoveLoc\",\"Timeout\"],\"Board\":{\"m\":[{\"cells\":{\"n\":\"List\",\"p\":{\"n\":\"List\",\"p\":{\"n\":\"Option\",\"p\":\"Player\"}}}}]},\"Final\":[\"Won\",\"Loss\",\"Tied\"],\"Loc\":{\"m\":[{\"x\":\"Int\"},{\"y\":\"Int\"}]},\"State\":{\"m\":[{\"board\":\"Board\"},{\"final\":{\"n\":\"Option\",\"p\":\"Final\"}}]},\"Bots\":{\"m\":[{\"x\":\"BotId\"},{\"o\":\"BotId\"}]},\"Init\":{\"m\":[{\"gameId\":\"GameId\"},{\"users\":\"Bots\"},{\"state\":\"State\"}]},\"Frame\":{\"m\":[{\"board\":\"Board\"},{\"loc\":\"Loc\"},{\"player\":\"Player\"}]},\"Result\":[\"Tie\",\"WinnerX\",\"WinnerO\"],\"Playback\":{\"m\":[{\"frames\":{\"n\":\"List\",\"p\":\"Frame\"}},{\"x\":\"BotId\"},{\"o\":\"BotId\"},{\"result\":\"Result\"}]},\"PostStart\":{\"o\":\"Init\"},\"PostMove\":{\"m\":[{\"loc\":\"Loc\"},{\"gameId\":\"GameId\"}],\"o\":\"State\"},\"GetPlayback\":{\"m\":[{\"gameId\":\"GameId\"}],\"o\":\"Playback\"}},\"version\":{\"major\":0,\"minor\":0}}"
