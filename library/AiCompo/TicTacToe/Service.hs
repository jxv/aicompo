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

type Step = TL.Step G.State G.Terminal
type Result = TL.Result GameId G.Player UserId G.State ()
type Starter = TL.Starter GameId G.Player UserId
type Lobby = TL.Lobby GameId G.Player UserId IO
type Sessions = TL.Sessions GameId UserId G.Player G.Input G.State G.Terminal IO
type SessionEntry = TL.SessionEntry G.Player G.Input G.State G.Terminal IO
type Registry = TL.Registry UserId TUser IO
type Results = TL.Results GameId G.Player UserId G.State () IO
type LabeledSession = TL.LabeledSession UserId G.Input G.State G.Terminal

data Components = Components
  { cLobby :: Lobby
  , cSessions :: Sessions
  , cRegistry :: Registry
  , cResults :: Results
  }

newComponents :: IO Components
newComponents = Components
  <$> TL.newLobbyFIFOWithSTM
  <*> TL.newSessionsWithSTM
  <*> (TL.newRegistryWithSTM (UserId <$> generateText64 32))
  <*> TL.newResultsWithSTM

--

postStart :: (MonadIO m, T0.TicTacToe'Thrower m) => Components -> UserId -> m T0.Init
postStart components userId = do
  let lobby = cLobby components
  let sessions = cSessions components
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

postMove :: (MonadIO m, T0.TicTacToe'Thrower m) => Components -> UserId -> T0.PostMove -> m T0.State
postMove components userId (T0.PostMove _loc gameId) = do
  let sessionId = GameId (toText gameId)
  let sessions = cSessions components
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

getStep :: (MonadIO m, T0.TicTacToe'Thrower m) => Sessions -> GameId -> UserId -> m Step
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

forkDispatcher :: Components -> IO ThreadId
forkDispatcher Components{cSessions,cResults,cLobby} = forkIO $ dispatcher (sessionWorker cSessions cResults) cLobby cSessions

dispatcher :: (Starter -> IO SessionEntry) -> Lobby -> Sessions -> IO ()
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

sessionWorker :: Sessions -> Results -> Starter -> IO SessionEntry
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
