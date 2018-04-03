module Util where

import qualified Data.List as List
import System.Random
import Data.Text (Text)
import Data.Text.Conversions (toText)
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad.Persist
import Database.Persist.Class (PersistEntityBackend, PersistEntity, BaseBackend, PersistField, EntityField)
import Database.Persist.Types (Filter(..), PersistFilter(..))

import qualified AiCompo.Authentication.Api.Major0 as V0

exact :: PersistField typ => EntityField record typ -> typ -> Filter record
exact attr val = Filter attr (Left val) Eq

notEqFilter :: PersistField typ => EntityField record typ -> typ -> Filter record
notEqFilter attr val = Filter attr (Left val) Ne

char64 :: [Char]
char64 = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_','-']

choice :: [a] -> IO a
choice xs = do
  x <- randomIO
  let idx = x `mod` length xs
  return $ xs List.!! idx

generateText64 :: Int -> IO Text
generateText64 n = fmap toText $ sequence $ replicate n $ choice char64

toDate :: UTCTime -> V0.Date
toDate utc = let (year, month, day) = toGregorian (utctDay utc) in V0.Date (fromIntegral year) month day

toDateTuple :: UTCTime -> (Int, Int, Int)
toDateTuple utc = let (year, month, day) = toGregorian (utctDay utc) in (fromIntegral year, month, day)

pamf :: Functor f => f a -> (a -> b) -> f b
pamf = flip fmap
