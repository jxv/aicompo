{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module DB.Generated where

import ClassyPrelude.Yesod hiding (Proxy)
import Database.Persist.Quasi

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (withPostgresqlConn, ConnectionString)

import Control.Monad.Persist (runSqlPersistT)
import Genesis.Persist (runMigrations)

import DB.NonGenerated

share [mkPersist sqlSettings]
  $(persistFileWith lowerCaseSettings "database/models")


migrate :: ConnectionString -> IO ()
migrate connStr = runStderrLoggingT $ withPostgresqlConn connStr $
  runSqlPersistT $(runMigrations "database")
