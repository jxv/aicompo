{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module DB.NonGenerated where

import Protolude
import Data.String (String)
import Data.List (lookup)
import Data.Text.Conversions
import Database.Persist.Sql (PersistField(..), PersistFieldSql(..), PersistValue)

fromPersistValueTagDefault :: (Bounded b, Enum b, ToText b) => PersistValue -> Either Text b
fromPersistValueTagDefault v = case fromPersistValue v of
  Left e -> Left e
  Right s -> case lookupTag (s :: String) of
    Nothing -> Left "Not a value"
    Just u -> Right u

lookupTag :: (ToText b, ToText a, Enum b, Bounded b) => a -> Maybe b
lookupTag tag = lookup (toText tag) [(toText v, v) | v <- [minBound..maxBound]]

data Role
  = Role'Admin
  | Role'Member
  deriving (Show, Eq, Enum, Bounded)

instance ToText Role where
  toText = \case
    Role'Admin -> "Admin"
    Role'Member -> "Member"

instance PersistField Role where
  toPersistValue x = toPersistValue (fromText (toText x) :: String)
  fromPersistValue = fromPersistValueTagDefault

instance PersistFieldSql Role where
  sqlType _ = sqlType (Proxy :: Proxy String)
