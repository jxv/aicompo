{-# LANGUAGE DeriveFunctor #-}
module AiCompo.TicTacToe.Game where

import qualified Data.Map as Map
import Data.Map (Map)

--

data Player
  = Player'X
  | Player'O
  deriving (Show, Eq, Enum)

data Loc = Loc
  { locX :: !Int
  , locY :: !Int
  } deriving (Show, Eq, Ord)

data Action = Action
  { actPlayer :: !Player
  , actLoc :: !Loc
  } deriving (Show, Eq)

data Board = Board
  { bCells :: !(Map Loc Player)
  , bSize :: !Int
  } deriving (Show, Eq)

data Result
  = Result'Unfinished
  | Result'Final FinalResult
  deriving (Show, Eq)

data FinalResult
  = FinalResult'Tie
  | FinalResult'Winner Player
  deriving (Show, Eq)

data State = State
  { sBoard :: Board
  , sFrames :: [(Board, Action)]
  } deriving (Show, Eq)

newState :: State
newState = State emptyBoard []

newtype Win a = Win a
  deriving (Eq, Show, Functor)

newtype Lose a = Lose a
  deriving (Eq, Show, Functor)

data Final
  = Final'Won
  | Final'Loss
  | Final'Tied
  deriving (Show, Eq)

--

game :: Play m => m ()
game = do
  initiate Player'X
  play Player'X Player'O

class Monad m => Play m where
  initiate :: Player -> m ()
  play :: Player -> Player -> m ()

play' :: (Interact m, BoardManager m, Play m) => Player -> Player -> m ()
play' p0 p1 = do
  loc <- move p0
  isValid <- isOpenLoc loc
  if isValid
    then do
      insertAtLoc loc p0
      res <- getResult
      case res of
        Result'Unfinished -> do
          endTurn p0
          play p1 p0
        Result'Final FinalResult'Tie -> tie
        Result'Final (FinalResult'Winner _) -> end (Win p0) (Lose p1)
    else forfeit (Win p1) (Lose p0)

class Monad m => Interact m where
  endTurn :: Player -> m ()
  move :: Player -> m Loc
  forfeit :: Win Player -> Lose Player -> m ()
  end :: Win Player -> Lose Player -> m ()
  tie :: m ()

class Monad m => BoardManager m where
  isOpenLoc :: Loc -> m Bool
  insertAtLoc :: Loc -> Player -> m ()
  getResult :: m Result

isOpenLoc' :: HasBoard m => Loc -> m Bool
isOpenLoc' loc = do
  board <- getBoard
  return $ valid loc board

valid :: Loc -> Board -> Bool
valid loc b = isLocInsideBoard && isCellEmpty
  where
    isLocInsideBoard = inside loc b
    isCellEmpty = not (Map.member loc (bCells b))

inside :: Loc -> Board -> Bool
inside loc b = locX loc >= 0 && locX loc < bSize b && locY loc >= 0 && locY loc < bSize b

insertPlayer :: Loc -> Player -> Board -> Board
insertPlayer loc player b
  | inside loc b = b { bCells = Map.insert loc player (bCells b) }
  | otherwise = b

insertAtLoc' :: HasBoard m => Loc -> Player -> m ()
insertAtLoc' loc p = do
  board <- getBoard
  putBoard $ insertPlayer loc p board

getResult' :: HasBoard m => m Result
getResult' = do
  board <- getBoard
  return $ result board
  where
    result :: Board -> Result
    result b
      | isWinner Player'X b = Result'Final $ FinalResult'Winner Player'X
      | isWinner Player'O b = Result'Final $ FinalResult'Winner Player'O
      | Map.size (bCells b) == (bSize b) ^ (2 :: Int) = Result'Final FinalResult'Tie
      | otherwise = Result'Unfinished
    isWinner :: Player -> Board -> Bool
    isWinner p b =
      or [all has [Loc x y | y <- q] | x <- q] ||
      or [all has [Loc x y | x <- q] | y <- q] ||
      all has [Loc z z | z <- q] ||
      all has [Loc z (bSize b - 1 - z) | z <- q]
      where
        has loc = Map.lookup loc (bCells b) == Just p
        q = indices b
    indices :: Board -> [Int]
    indices b = [0 .. bSize b - 1]

class Monad m => HasBoard m where
  getBoard :: m Board
  putBoard :: Board -> m ()

emptyBoard :: Board
emptyBoard = Board Map.empty 3

getOpponent :: Player -> Player
getOpponent Player'X = Player'O
getOpponent Player'O = Player'X
