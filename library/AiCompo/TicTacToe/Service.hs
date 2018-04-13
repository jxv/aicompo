module AiCompo.TicTacToe.Service where

import Control.Monad.Persist (MonadPersist(insert_, getBy, selectList), runSqlPoolPersistT)
import Control.Monad (forever)
import Control.Applicative ((<|>))
import Control.Concurrent (readChan, writeChan, forkIO, killThread, ThreadId)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader(..), asks)
import Control.Monad.State (StateT, evalStateT, MonadState, modify, gets, put, get)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans (lift)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.String (IsString(..))
import Data.Time.Clock (getCurrentTime)
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.Text.Conversions (ToText(..),FromText(..))
import Data.Traversable (sequence)
import Database.Persist.Sql (ConnectionPool)
import Database.Persist.Types (SelectOpt(..), Entity(..))

import qualified TurnLoop.Types as TL
import qualified TurnLoop.STM as TL

import System.Random
import Data.Text.Conversions (toText)

import qualified DB
import qualified AiCompo.TicTacToe.Api.Major0 as T0
import qualified AiCompo.TicTacToe.Game as G

import Util (exact)

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

newtype BotId = BotId Text
  deriving (Show, Eq, Ord, IsString, ToText, FromText)

generateBotId :: IO BotId
generateBotId = BotId <$> generateText64 32

--

data TBot = TBot

--

type Step = TL.Step G.State G.Final
type Result = TL.Result GameId G.Player BotId G.State G.FinalResult
type Starter = TL.Starter GameId G.Player BotId
type Lobby = TL.Lobby GameId G.Player BotId IO
type Session = TL.Session G.Loc G.State G.Final
type Sessions = TL.Sessions GameId BotId G.Player G.Loc G.State G.Final IO
type SessionEntry = TL.SessionEntry G.Player G.Loc G.State G.Final IO
type Registry = TL.Registry BotId TBot IO
type Results = TL.Results GameId G.Player BotId G.State G.FinalResult IO
type LabeledSession = TL.LabeledSession BotId G.Loc G.State G.Final
type Components = TL.Components GameId G.Player BotId TBot G.Loc G.State G.FinalResult G.Final IO

newComponents :: ConnectionPool -> IO Components
newComponents pool = do
  components <- TL.newComponentsWithSTM (BotId <$> generateText64 32)
  return $ components
    { TL.cResults = TL.Results
        { TL.rSaveResult = saveResult' pool
        , TL.rFindResult = findResult' pool } }

--

saveResult' :: ConnectionPool -> Result -> IO ()
saveResult' pool TL.Result{TL.rStarter,TL.rState,TL.rExtra} = do
  now <- getCurrentTime
  let (GameId gameId) = TL.sSessionId rStarter
  let (BotId xBotId) = TL.sUserIds rStarter G.Player'X
  let (BotId oBotId) = TL.sUserIds rStarter G.Player'O
  let result = finalResultToText rExtra
  let playback = DB.TicTacToePlayback now gameId (boardToText $ G.sBoard rState) xBotId oBotId result
  let frames = flip map (zip [0..] $ G.sFrames rState) $ \(idx, (board, action)) -> DB.TicTacToeFrame
        gameId
        idx
        (boardToText board)
        (G.locX $ G.actLoc action)
        (G.locY $ G.actLoc action)
        (toText $ [toCellChar $ G.actPlayer action])
  flip runSqlPoolPersistT pool $ do
    insert_ playback
    mapM_ insert_ frames

findResult' :: ConnectionPool -> GameId -> IO (Maybe Result)
findResult' pool (GameId gameId) = flip runSqlPoolPersistT pool $ runMaybeT $ do
  Entity{entityVal} <- MaybeT $ getBy (DB.UniqueTicTacToePlaybackGameId gameId)
  let starter = TL.Starter (GameId gameId) $ \case
        G.Player'X -> BotId $ DB.ticTacToePlaybackXBot entityVal
        G.Player'O -> BotId $ DB.ticTacToePlaybackOBot entityVal
  board <- MaybeT . return $ textToBoardMay (DB.ticTacToePlaybackBoard entityVal)
  result <- MaybeT . return $ textToFinalResultMay (DB.ticTacToePlaybackResult entityVal)
  frames <- lift $ selectList [exact DB.TicTacToeFrameGameId gameId] [Asc DB.TicTacToeFrameIndex]
  frames' <- flip mapM frames $ \Entity{entityVal=frame} -> do
    board' <- MaybeT . return $ textToBoardMay (DB.ticTacToeFrameBoard frame)
    player <- MaybeT . return $ textToPlayer (DB.ticTacToeFramePlayer frame)
    let loc = G.Loc (DB.ticTacToeFrameLocX frame) (DB.ticTacToeFrameLocY frame)
    let action = G.Action player loc
    return (board', action)
  let state = G.State board frames'
  return $ TL.Result starter state result

boardToText :: G.Board -> Text
boardToText G.Board{G.bCells} = toText $ [toCellCharMay $ Map.lookup (G.Loc x y) bCells | y <- [0..2], x <- [0..2]]

finalResultToText :: G.FinalResult -> Text
finalResultToText = \case
  G.FinalResult'Tie -> "Tie"
  G.FinalResult'Winner p -> case p of
    G.Player'X -> "WinnerX"
    G.Player'O -> "WinnerO"

textToFinalResultMay :: Text -> Maybe G.FinalResult
textToFinalResultMay = \case
  "Tie" -> Just G.FinalResult'Tie
  "WinnerX" -> Just $ G.FinalResult'Winner G.Player'X
  "WinnerO" -> Just $ G.FinalResult'Winner G.Player'O
  _ -> Nothing

toCellCharMay :: Maybe G.Player -> Char
toCellCharMay = maybe ' ' toCellChar

toCellChar :: G.Player -> Char
toCellChar G.Player'X = 'X'
toCellChar G.Player'O = 'O'

textToPlayer :: Text -> Maybe G.Player
textToPlayer = \case
  "X" -> Just G.Player'X
  "O" -> Just G.Player'O
  _ -> Nothing

charToCell :: Char -> Maybe (Maybe G.Player)
charToCell = \case
  'X' -> Just $ Just G.Player'X
  'O' -> Just $ Just G.Player'O
  ' ' -> Just $ Nothing
  _ -> Nothing

textToBoardMay :: Text -> Maybe G.Board
textToBoardMay s
  | length stringCells == 9 = do
      cells <- sequence $ map charToCell stringCells
      let cells' = catMaybes cells
      Just . G.Board . Map.fromList $ zip
        [ G.Loc x y | y <- [0..2], x <- [0..2] ]
        cells'
  | otherwise = Nothing
  where
    stringCells = fromText s

--

postStart :: (MonadIO m, T0.TicTacToe'Thrower m) => Components -> Maybe BotId -> m T0.Init
postStart _ Nothing = T0.ticTacToe'throw T0.Error'Unauthorized
postStart components (Just botId) = do
  let lobby = TL.cLobby components
  let sessions = TL.cSessions components
  starter' <- liftIO $ TL.lTransferUser lobby botId
  case starter' of
    Nothing -> T0.ticTacToe'throw T0.Error'Timeout
    Just TL.Starter{TL.sSessionId,TL.sUserIds} -> do
      step <- getStep sessions sSessionId botId
      let gameId = T0.GameId (toText sSessionId)
      let bots = T0.Bots { T0.botsO = botIdO, T0.botsX = botIdX }
          botIdX = T0.BotId (toText $ sUserIds G.Player'X)
          botIdO = T0.BotId (toText $ sUserIds G.Player'O)
      return $ T0.Init gameId bots (gameStepToApiState step)

postMove :: (MonadIO m, T0.TicTacToe'Thrower m) => Components -> Maybe BotId -> T0.PostMove -> m T0.State
postMove _ Nothing _ = T0.ticTacToe'throw T0.Error'Unauthorized
postMove components (Just botId) (T0.PostMove loc gameId) = do
  let sessionId = GameId (toText gameId)
  let sessions = TL.cSessions components
  sessionRecord' <- liftIO $ TL.sFindSession sessions sessionId
  case sessionRecord' of
    Nothing -> T0.ticTacToe'throw T0.Error'GameId
    Just sessionRecord -> do
      case unrep [G.Player'X,G.Player'O] TL.lsUserId (TL.srLabeled sessionRecord) botId of
        Nothing -> T0.ticTacToe'throw T0.Error'Unauthorized -- Non-existent bot id
        Just labeledSession -> do
          let session = TL.lsSession labeledSession
          liftIO $ writeChan (TL.sInput session) (G.Loc (T0.locX loc) (T0.locY loc))
          step <- liftIO $ readChan (TL.sStep session)
          return (gameStepToApiState step)

getPlayback :: (MonadIO m, T0.TicTacToe'Thrower m) => Components -> Maybe BotId -> T0.GetPlayback -> m T0.Playback
getPlayback components _ (T0.GetPlayback gameId) = do
  let sessionId = GameId (toText gameId)
  result' <- liftIO $ TL.rFindResult (TL.cResults components) sessionId
  case result' of
    Nothing -> T0.ticTacToe'throw T0.Error'GameId
    Just result -> do
      let botIds p = T0.BotId . toText $ TL.sUserIds (TL.rStarter result) p
      let x = botIds G.Player'X
      let o = botIds G.Player'O
      let frames = map gameFrameToApiFrame (G.sFrames $ TL.rState result)
      let apiResult = gameFinalResultToApiResult $ TL.rExtra result
      return $ T0.Playback frames x o apiResult

--

unrep :: [player] -> (session -> BotId) -> (player -> session) -> BotId -> Maybe session
unrep players sessionToBotId rep botId = List.foldl' (<|>) Nothing $ map
  (\p -> if botId == sessionToBotId (rep p) then Just (rep p) else Nothing)
  players

gameFrameToApiFrame :: (G.Board, G.Action) -> T0.Frame
gameFrameToApiFrame (board, action) = T0.Frame
  (gameBoardToApiBoard board)
  (gameLocToApiLoc $ G.actLoc action)
  (gamePlayerToApiPlayer $ G.actPlayer action)

gameFinalToApiFinal :: G.Final -> T0.Final
gameFinalToApiFinal = \case
  G.Final'Won -> T0.Final'Won
  G.Final'Loss -> T0.Final'Loss
  G.Final'Tied -> T0.Final'Tied

gameBoardToApiBoard :: G.Board -> T0.Board
gameBoardToApiBoard b = T0.Board [ [ gamePlayerToApiPlayer <$> Map.lookup (G.Loc x y) (G.bCells b) | x <- [0..2]] | y <- [0..2] ]

gamePlayerToApiPlayer :: G.Player -> T0.Player
gamePlayerToApiPlayer = \case
  G.Player'X -> T0.Player'X
  G.Player'O -> T0.Player'O

gameStepToApiState :: Step -> T0.State
gameStepToApiState (TL.Step st terminal) = T0.State
  (gameBoardToApiBoard $ G.sBoard st)
  (gameFinalToApiFinal <$> terminal)

gameFinalResultToApiResult :: G.FinalResult -> T0.Result
gameFinalResultToApiResult = \case
  G.FinalResult'Tie -> T0.Result'Tie
  G.FinalResult'Winner G.Player'X -> T0.Result'WinnerX
  G.FinalResult'Winner G.Player'O -> T0.Result'WinnerO

gameLocToApiLoc :: G.Loc -> T0.Loc
gameLocToApiLoc loc = T0.Loc (G.locX loc) (G.locY loc)

--

getStep :: (MonadIO m, T0.TicTacToe'Thrower m) => Sessions -> GameId -> BotId -> m Step
getStep sessions gameId botId = do
  sessionRecord' <- liftIO $ TL.sFindSession sessions gameId
  case sessionRecord' of
    Nothing -> T0.ticTacToe'throw T0.Error'GameId
    Just sessionRecord -> do
      let TL.LabeledSession botIdX sessionX = TL.srLabeled sessionRecord G.Player'X
      let TL.LabeledSession botIdO sessionO = TL.srLabeled sessionRecord G.Player'O
      let x = if botIdX == botId then Just sessionX else Nothing
      let o = if botIdO == botId then Just sessionO else Nothing
      case x <|> o of
        Nothing -> T0.ticTacToe'throw T0.Error'Unauthorized -- Can't find by bot id
        Just session -> liftIO $ readChan $ TL.sStep session

forkDispatcher :: Components -> IO ThreadId
forkDispatcher TL.Components{TL.cSessions,TL.cResults,TL.cLobby} = forkIO $ dispatcher (sessionWorker cSessions cResults) cLobby cSessions

dispatcher :: (Starter -> IO SessionEntry) -> Lobby -> Sessions -> IO ()
dispatcher dispatchSession lobby sessions = forever $ do
  gameId <- generateGameId
  botIdX <- popBot gameId
  botIdO <- popBot gameId
  let playerToBotId = \case G.Player'X -> botIdX; G.Player'O -> botIdO
  let starter = TL.Starter gameId playerToBotId
  TL.SessionEntry thread playerToSessions <- dispatchSession starter
  let sessionRecord = TL.SessionRecord thread $ \xo -> TL.LabeledSession (playerToBotId xo) (playerToSessions xo)
  TL.sInsertSession sessions (gameId, sessionRecord)
  TL.lAnnounceSession lobby starter
  where
    popBot gameId = do
      botId' <- TL.lDequeueUser lobby gameId
      case botId' of
        Nothing -> popBot gameId
        Just botId -> return botId

sessionWorker :: Sessions -> Results -> Starter -> IO SessionEntry
sessionWorker sessions results starter = do
  sessionX <- TL.newSessionWithChan
  sessionO <- TL.newSessionWithChan

  threadId <- forkIO . forever $ do
    let config = GameConfig
          starter
          (\case G.Player'X -> sessionX; G.Player'O -> sessionO)
          sessions
          results
    runGame G.game (config, G.newState)

  let thread = TL.Thread threadId (killThread threadId)
  return $ TL.SessionEntry thread $ \case G.Player'X -> sessionX; G.Player'O -> sessionO

--

newtype Game a = Game (ReaderT GameConfig (StateT G.State IO) a)
  deriving (Functor, Applicative, Monad, MonadReader GameConfig, MonadState G.State, MonadIO)

data GameConfig = GameConfig
  { gcStarter :: Starter
  , gcGetSession :: G.Player -> Session
  , gcSessions :: Sessions
  , gcResults :: Results
  }

runGame :: Game a -> (GameConfig, G.State) -> IO a
runGame (Game m) (config, st) = evalStateT (runReaderT m config) st

instance G.Play Game where
  initiate = initiate'
  play = G.play'

instance G.Interact Game where
  endTurn = endTurn'
  move = move'
  forfeit = forfeit'
  end = end'
  tie = tie'

instance G.BoardManager Game where
  isOpenLoc = G.isOpenLoc'
  insertAtLoc = insertAtLoc'
  getResult = G.getResult'

instance G.HasBoard Game where
  getBoard = gets G.sBoard
  putBoard b = modify $ \s -> s { G.sBoard = b }

initiate' :: (MonadIO m, MonadReader GameConfig m, MonadState G.State m) => G.Player -> m ()
initiate' player = do
  st <- get
  gc <- ask
  liftIO $ writeChan (TL.sStep $ gcGetSession gc player) (TL.Step st Nothing)

move' :: (MonadIO m, MonadReader GameConfig m, MonadState G.State m) => G.Player -> m G.Loc
move' player = do
  chan <- asks (\gc -> TL.sInput $ gcGetSession gc player)
  liftIO $ readChan chan

end' :: (MonadIO m, MonadReader GameConfig m, MonadState G.State m) => G.Win G.Player -> G.Lose G.Player -> m ()
end' (G.Win w) (G.Lose l) = do
  st <- get
  gc <- ask
  liftIO $ writeChan (TL.sStep $ gcGetSession gc w) (TL.Step st (Just G.Final'Won))
  liftIO $ writeChan (TL.sStep $ gcGetSession gc l) (TL.Step st (Just G.Final'Loss))
  liftIO $ TL.rSaveResult (gcResults gc) $ TL.Result (gcStarter gc) st (G.FinalResult'Winner w)

forfeit' :: (MonadIO m, MonadReader GameConfig m, MonadState G.State m) => G.Win G.Player -> G.Lose G.Player -> m ()
forfeit' (G.Win w) (G.Lose l) = do
  st <- get
  gc <- ask
  liftIO $ writeChan (TL.sStep $ gcGetSession gc w) (TL.Step st (Just G.Final'Won))
  liftIO $ writeChan (TL.sStep $ gcGetSession gc l) (TL.Step st (Just G.Final'Loss))
  liftIO $ TL.rSaveResult (gcResults gc) $ TL.Result (gcStarter gc) st (G.FinalResult'Winner w)

tie' :: (MonadIO m, MonadReader GameConfig m, MonadState G.State m) => m ()
tie' = do
  st <- get
  gc <- ask
  liftIO $ writeChan (TL.sStep $ gcGetSession gc G.Player'X) (TL.Step st (Just G.Final'Tied))
  liftIO $ writeChan (TL.sStep $ gcGetSession gc G.Player'O) (TL.Step st (Just G.Final'Tied))
  liftIO $ TL.rSaveResult (gcResults gc) $ TL.Result (gcStarter gc) st (G.FinalResult'Tie)

insertAtLoc' :: (MonadIO m, MonadReader GameConfig m, MonadState G.State m) => G.Loc -> G.Player -> m ()
insertAtLoc' loc player = do
  st <- get
  let board' = G.insertPlayer loc player (G.sBoard st)
  let st' = G.State board' (G.sFrames st ++ [(board', G.Action player loc)])
  put st'

endTurn' :: (MonadIO m, MonadReader GameConfig m, MonadState G.State m) => G.Player -> m ()
endTurn' player = do
  st <- get
  let step = TL.Step st Nothing
  let opponent = G.getOpponent player
  chan <- asks (\gc -> TL.sStep $ gcGetSession gc opponent)
  liftIO $ writeChan chan step
