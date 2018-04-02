module CompoAi where

import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent
import Data.String (IsString(..))
import Fluid.Types (Pull(..), defHooks, Hooks(..))
import Fluid.Server.Scotty (runServer, ScottyT)
import Fluid.Server (ServiceThrower, MonadCatch ,MonadThrow)
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import System.Environment
import Safe (readMay)
import Data.Maybe (fromMaybe)
import Data.Text.Conversions (ToText(..),FromText(..))

import TurnLoop.Types
import TurnLoop.STM

import qualified CompoAi.Api.TicTacToe.Server as T
import qualified CompoAi.Api.TicTacToe.Major0 as T0
import qualified Data.List as List

--

import System.Random
import Data.Text (Text)
import Data.Text.Conversions (toText)

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

data Player
  = Player'X
  | Player'O
  deriving (Show, Eq, Enum)

newtype UserId = UserId Text
  deriving (Show, Eq, Ord, IsString, ToText, FromText)

generateUserId :: IO UserId
generateUserId = UserId <$> generateText64 32

--

data TInput = TInput
data TState = TState
data TTerminal = TTerminal
data TUser = TUser

--

type TStep = Step TState TTerminal
type TResult = Result GameId Player UserId TState ()
type TStarter = Starter GameId Player UserId
type TLobby = Lobby GameId Player UserId IO
type TSessions = Sessions GameId UserId Player TInput TState TTerminal IO
type TSessionEntry = SessionEntry Player TInput TState TTerminal IO
type TRegistry = Registry UserId TUser IO
type TResults = Results GameId Player UserId TState () IO

data Config = Config
  { _cTicTacToe :: TicTacToe
  }

data TicTacToe = TicTacToe
  { _tLobby :: TLobby
  , _tSessions :: TSessions
  , _tRegistry :: TRegistry
  , _tResults :: TResults
  }

newTicTacToe :: IO TicTacToe
newTicTacToe = TicTacToe
  <$> newLobbyFIFOWithSTM
  <*> newSessionsWithSTM
  <*> (newRegistryWithSTM (return ""))
  <*> newResultsWithSTM

--

newtype App a = App { unApp :: ReaderT Config IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadReader Config)

runApp :: Config -> App a -> IO a
runApp config (App m) = runReaderT m config

instance ServiceThrower App
instance T0.TicTacToe'Thrower App

instance T0.TicTacToe'Service UserId App where
  ticTacToe'PostStart = postStart
  ticTacToe'PostMove = postMove

postStart :: UserId -> App T0.Init
postStart userId = do
  lobby <- asks (_tLobby . _cTicTacToe)
  starter' <- liftIO $ lTransferUser lobby userId
  case starter' of
    Nothing -> T0.ticTacToe'throw T0.Error'Timeout
    Just Starter{sSessionId,sUserIds} -> do
      let gameId = T0.GameId (toText sSessionId)
      let userIdX = T0.UserId (toText $ sUserIds Player'X)
      let userIdO = T0.UserId (toText $ sUserIds Player'O)
      let users = T0.Users { T0.userso = userIdO, T0.usersx = userIdX }
      return $ T0.Init gameId users (T0.State (T0.Board emptyBoard) Nothing)

postMove :: UserId -> T0.PostMove -> App T0.State
postMove _ (T0.PostMove loc gameToken) = do
  return $ T0.State (T0.Board emptyBoard) Nothing

--

emptyBoard :: [[Maybe T.Player]]
emptyBoard = [[Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing]]

main :: IO ()
main = do
  port' <- lookupEnv "PORT"
  ticTacToe <- newTicTacToe
  let config = Config ticTacToe
  _ <- forkIO $ ticTacToeDispatcher (ticTacToeSessionWorker (_tSessions ticTacToe) (_tResults ticTacToe)) (_tLobby ticTacToe) (_tSessions ticTacToe)
  runServer
    T.ticTacToe'pull { port = fromMaybe (port T.ticTacToe'pull) (readMay =<< port') }
    (runApp config)
    routes

ticTacToeDispatcher :: (TStarter -> IO TSessionEntry) -> TLobby -> TSessions -> IO ()
ticTacToeDispatcher dispatchSession lobby sessions = forever $ do
  gameId <- generateGameId
  userIdX <- popUser gameId
  userIdO <- popUser gameId
  let playerToUserId = \case Player'X -> userIdX; Player'O -> userIdO
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

ticTacToeSessionWorker :: TSessions -> TResults -> TStarter -> IO TSessionEntry
ticTacToeSessionWorker sessions results Starter{sSessionId,sUserIds} = do
  sessionX <- newSessionWithChan
  sessionO <- newSessionWithChan
  threadId <- forkIO $ do
    return ()
  let thread = Thread threadId (killThread threadId)
  return $ SessionEntry thread $ \case
    Player'X -> sessionX
    Player'O -> sessionO

routes :: ScottyT TL.Text App ()
routes = do
  T.ticTacToe'Scotty'Post T.ticTacToe'pull (const $ defHooks {metaMiddleware = ticTacToeMetaMiddleware0})
  T.ticTacToe'Scotty'Get T.ticTacToe'pull

ticTacToeMetaMiddleware0 :: T0.AccessToken -> App UserId
ticTacToeMetaMiddleware0 (T0.AccessToken accessToken') = return $ UserId accessToken'
