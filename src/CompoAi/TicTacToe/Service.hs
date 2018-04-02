module CompoAi.TicTacToe.Service where

import Control.Monad (forever)
import Control.Applicative ((<|>), (<*>))
import Control.Concurrent (readChan, writeChan, forkIO, killThread, ThreadId)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.List as List
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Conversions (ToText(..),FromText(..))
import TurnLoop.Types
import TurnLoop.STM

import System.Random
import Data.Text.Conversions (toText)

import qualified CompoAi.TicTacToe.Api.Server as T
import qualified CompoAi.TicTacToe.Api.Major0 as T0
import qualified CompoAi.TicTacToe.Game as Game

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

type TStep = Step Game.State Game.Terminal
type TResult = Result GameId Game.Player UserId Game.State ()
type TStarter = Starter GameId Game.Player UserId
type TLobby = Lobby GameId Game.Player UserId IO
type TSessions = Sessions GameId UserId Game.Player Game.Input Game.State Game.Terminal IO
type TSessionEntry = SessionEntry Game.Player Game.Input Game.State Game.Terminal IO
type TRegistry = Registry UserId TUser IO
type TResults = Results GameId Game.Player UserId Game.State () IO
type TLabeledSession = LabeledSession UserId Game.Input Game.State Game.Terminal

data TObjects = TObjects
  { tLobby :: TLobby
  , tSessions :: TSessions
  , tRegistry :: TRegistry
  , tResults :: TResults
  }

newTObjects :: IO TObjects
newTObjects = TObjects
  <$> newLobbyFIFOWithSTM
  <*> newSessionsWithSTM
  <*> (newRegistryWithSTM (UserId <$> generateText64 32))
  <*> newResultsWithSTM

--

postStart :: (MonadIO m, T0.TicTacToe'Thrower m) => TObjects -> UserId -> m T0.Init
postStart ticTacToe userId = do
  let lobby = tLobby ticTacToe
  let sessions = tSessions ticTacToe
  starter' <- liftIO $ lTransferUser lobby userId
  case starter' of
    Nothing -> T0.ticTacToe'throw T0.Error'Timeout
    Just Starter{sSessionId,sUserIds} -> do
      step <- getStep sessions sSessionId userId

      let gameId = T0.GameId (toText sSessionId)
      let users = T0.Users { T0.userso = userIdO, T0.usersx = userIdX }
          userIdX = T0.UserId (toText $ sUserIds Game.Player'X)
          userIdO = T0.UserId (toText $ sUserIds Game.Player'O)
      let terminal = case sTerminal step of
            Nothing -> Nothing
            Just _ -> Nothing
      let board = case sState step of
            Game.State -> emptyBoard

      return $ T0.Init gameId users (T0.State (T0.Board board) terminal)

postMove :: (MonadIO m, T0.TicTacToe'Thrower m) => TObjects -> UserId -> T0.PostMove -> m T0.State
postMove ticTacToe userId (T0.PostMove _loc gameId) = do
  let sessionId = GameId (toText gameId)
  let sessions = tSessions ticTacToe
  sessionRecord' <- liftIO $ sFindSession sessions sessionId
  case sessionRecord' of
    Nothing -> T0.ticTacToe'throw T0.Error'GameId
    Just sessionRecord -> do
      case unrep [Game.Player'X,Game.Player'O] lsUserId (srLabeled sessionRecord) userId of
        Nothing -> T0.ticTacToe'throw T0.Error'Unauthorized -- Non-existent user id
        Just labeledSession -> do
          let session = lsSession labeledSession
          liftIO $ writeChan (sInput session) Game.Input
          _ <- liftIO $ readChan (sStep session)
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
  sessionRecord' <- liftIO $ sFindSession sessions gameId
  case sessionRecord' of
    Nothing -> T0.ticTacToe'throw T0.Error'GameId
    Just sessionRecord -> do
      let LabeledSession userIdX sessionX = srLabeled sessionRecord Game.Player'X
      let LabeledSession userIdO sessionO = srLabeled sessionRecord Game.Player'O
      let x = if userIdX == userId then Just sessionX else Nothing
      let o = if userIdO == userId then Just sessionO else Nothing
      case x <|> o of
        Nothing -> T0.ticTacToe'throw T0.Error'Unauthorized -- Can't find by user id
        Just session -> liftIO $ readChan $ sStep session

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
  let playerToUserId = \case Game.Player'X -> userIdX; Game.Player'O -> userIdO
  let starter = Starter gameId playerToUserId
  SessionEntry thread playerToSessions <- dispatchSession starter
  let sessionRecord = SessionRecord thread $ \xo -> LabeledSession (playerToUserId xo) (playerToSessions xo)
  sInsertSession sessions (gameId, sessionRecord)
  lAnnounceSession lobby starter
  where
    popUser gameId = do
      userId' <- lDequeueUser lobby gameId
      case userId' of
        Nothing -> popUser gameId
        Just userId -> return userId

sessionWorker :: TSessions -> TResults -> TStarter -> IO TSessionEntry
sessionWorker _sessions _results Starter{} = do
  sessionX <- newSessionWithChan
  sessionO <- newSessionWithChan

  threadId <- forkIO . forever $ do
    writeChan (sStep sessionX) $ Step { sTerminal = Nothing, sState = Game.State }
    _ <- readChan (sInput sessionX)
    writeChan (sStep sessionO) $ Step { sTerminal = Nothing, sState = Game.State }
    _ <- readChan (sInput sessionO)
    return ()

  let thread = Thread threadId (killThread threadId)
  return $ SessionEntry thread $ \case Game.Player'X -> sessionX; Game.Player'O -> sessionO
