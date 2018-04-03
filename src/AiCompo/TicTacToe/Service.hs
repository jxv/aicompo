module AiCompo.TicTacToe.Service where

import Control.Monad (forever)
import Control.Applicative ((<|>), (<*>))
import Control.Concurrent (readChan, writeChan, forkIO, killThread, ThreadId)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.List as List
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Conversions (ToText(..),FromText(..))

import qualified TurnLoop.Types as TL
import qualified TurnLoop.STM as TL

import System.Random
import Data.Text.Conversions (toText)

import qualified AiCompo.TicTacToe.Api.Server as T
import qualified AiCompo.TicTacToe.Api.Major0 as T0
import qualified AiCompo.TicTacToe.Game as G

char64 :: [Char]
char64 = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

choice :: [a] -> IO a
choice xs = do
  x <- randomIO
  let idx = x `mod` length xs
  return $ xs List.!! idx

generateText64 :: Int -> IO Text
generateText64 n = fmap toText $ sequence $ replicate n $ choice char64

--

newtype GameId = GameId Text
  deriving (Show, Eq, Ord, IsString, ToText, FromText)

generateGameId :: IO GameId
generateGameId = GameId <$> generateText64 32

--

newtype UserId = UserId Text
  deriving (Show, Eq, Ord, IsString, ToText, FromText)

generateUserId :: IO UserId
generateUserId = UserId <$> generateText64 32

--

data TUser = TUser

--

type TStep = TL.Step G.State G.Terminal
type TResult = TL.Result GameId G.Player UserId G.State ()
type TStarter = TL.Starter GameId G.Player UserId
type TLobby = TL.Lobby GameId G.Player UserId IO
type TSessions = TL.Sessions GameId UserId G.Player G.Input G.State G.Terminal IO
type TSessionEntry = TL.SessionEntry G.Player G.Input G.State G.Terminal IO
type TRegistry = TL.Registry UserId TUser IO
type TResults = TL.Results GameId G.Player UserId G.State () IO
type TLabeledSession = TL.LabeledSession UserId G.Input G.State G.Terminal

data TObjects = TObjects
  { tLobby :: TLobby
  , tSessions :: TSessions
  , tRegistry :: TRegistry
  , tResults :: TResults
  }

newTObjects :: IO TObjects
newTObjects = TObjects
  <$> TL.newLobbyFIFOWithSTM
  <*> TL.newSessionsWithSTM
  <*> (TL.newRegistryWithSTM (UserId <$> generateText64 32))
  <*> TL.newResultsWithSTM

--

postStart :: (MonadIO m, T0.TicTacToe'Thrower m) => TObjects -> UserId -> m T0.Init
postStart ticTacToe userId = do
  let lobby = tLobby ticTacToe
  let sessions = tSessions ticTacToe
  starter' <- liftIO $ TL.lTransferUser lobby userId
  case starter' of
    Nothing -> T0.ticTacToe'throw T0.Error'Timeout
    Just TL.Starter{TL.sSessionId,TL.sUserIds} -> do
      step <- getStep sessions sSessionId userId

      let gameId = T0.GameId (toText sSessionId)
      let users = T0.Users { T0.userso = userIdO, T0.usersx = userIdX }
          userIdX = T0.UserId (toText $ sUserIds G.Player'X)
          userIdO = T0.UserId (toText $ sUserIds G.Player'O)
      let terminal = case TL.sTerminal step of
            Nothing -> Nothing
            Just _ -> Nothing
      let board = case TL.sState step of
            G.State -> emptyBoard

      return $ T0.Init gameId users (T0.State (T0.Board board) terminal)

postMove :: (MonadIO m, T0.TicTacToe'Thrower m) => TObjects -> UserId -> T0.PostMove -> m T0.State
postMove ticTacToe userId (T0.PostMove _loc gameId) = do
  let sessionId = GameId (toText gameId)
  let sessions = tSessions ticTacToe
  sessionRecord' <- liftIO $ TL.sFindSession sessions sessionId
  case sessionRecord' of
    Nothing -> T0.ticTacToe'throw T0.Error'GameId
    Just sessionRecord -> do
      case unrep [G.Player'X,G.Player'O] TL.lsUserId (TL.srLabeled sessionRecord) userId of
        Nothing -> T0.ticTacToe'throw T0.Error'Unauthorized -- Non-existent user id
        Just labeledSession -> do
          let session = TL.lsSession labeledSession
          liftIO $ writeChan (TL.sInput session) G.Input
          _ <- liftIO $ readChan (TL.sStep session)
          return $ T0.State (T0.Board emptyBoard) Nothing

unrep :: [player] -> (session -> UserId) -> (player -> session) -> UserId -> Maybe session
unrep players sessionToUserId rep userId = List.foldl' (<|>) Nothing $ map
  (\p -> if userId == sessionToUserId (rep p) then Just (rep p) else Nothing)
  players

--

emptyBoard :: [[Maybe T.Player]]
emptyBoard = [[Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing]]

getStep :: (MonadIO m, T0.TicTacToe'Thrower m) => TSessions -> GameId -> UserId -> m TStep
getStep sessions gameId userId = do
  sessionRecord' <- liftIO $ TL.sFindSession sessions gameId
  case sessionRecord' of
    Nothing -> T0.ticTacToe'throw T0.Error'GameId
    Just sessionRecord -> do
      let TL.LabeledSession userIdX sessionX = TL.srLabeled sessionRecord G.Player'X
      let TL.LabeledSession userIdO sessionO = TL.srLabeled sessionRecord G.Player'O
      let x = if userIdX == userId then Just sessionX else Nothing
      let o = if userIdO == userId then Just sessionO else Nothing
      case x <|> o of
        Nothing -> T0.ticTacToe'throw T0.Error'Unauthorized -- Can't find by user id
        Just session -> liftIO $ readChan $ TL.sStep session

forkDispatcher :: TObjects -> IO ThreadId
forkDispatcher ticTacToe = forkIO $ dispatcher
  (sessionWorker (tSessions ticTacToe) (tResults ticTacToe))
  (tLobby ticTacToe)
  (tSessions ticTacToe)

dispatcher :: (TStarter -> IO TSessionEntry) -> TLobby -> TSessions -> IO ()
dispatcher dispatchSession lobby sessions = forever $ do
  gameId <- generateGameId
  userIdX <- popUser gameId
  userIdO <- popUser gameId
  let playerToUserId = \case G.Player'X -> userIdX; G.Player'O -> userIdO
  let starter = TL.Starter gameId playerToUserId
  TL.SessionEntry thread playerToSessions <- dispatchSession starter
  let sessionRecord = TL.SessionRecord thread $ \xo -> TL.LabeledSession (playerToUserId xo) (playerToSessions xo)
  TL.sInsertSession sessions (gameId, sessionRecord)
  TL.lAnnounceSession lobby starter
  where
    popUser gameId = do
      userId' <- TL.lDequeueUser lobby gameId
      case userId' of
        Nothing -> popUser gameId
        Just userId -> return userId

sessionWorker :: TSessions -> TResults -> TStarter -> IO TSessionEntry
sessionWorker _sessions _results TL.Starter{} = do
  sessionX <- TL.newSessionWithChan
  sessionO <- TL.newSessionWithChan

  threadId <- forkIO . forever $ do
    writeChan (TL.sStep sessionX) $ TL.Step { TL.sTerminal = Nothing, TL.sState = G.State }
    _ <- readChan (TL.sInput sessionX)
    writeChan (TL.sStep sessionO) $ TL.Step { TL.sTerminal = Nothing, TL.sState = G.State }
    _ <- readChan (TL.sInput sessionO)
    return ()

  let thread = TL.Thread threadId (killThread threadId)
  return $ TL.SessionEntry thread $ \case G.Player'X -> sessionX; G.Player'O -> sessionO
