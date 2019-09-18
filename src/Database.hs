{-# LANGUAGE OverloadedStrings #-}
module Database
    ( dbMigration
    , insertTrend
    )
where

-- Used in the `withDbRun` type signature
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Control
import           Control.Monad.Logger

-- interfacing with Sqlite
import           Database.Persist
import           Database.Persist.Class
import           Database.Persist.Sqlite       as DbSql

import           Models
import           Data.Text

import           Control.Exception              ( SomeException )
import           Control.Monad.Catch

dbName :: Text
dbName = "twitter.db"

withDbRun :: SqlPersistT (NoLoggingT (ResourceT IO)) b -> IO b
withDbRun = runSqlite dbName

-- ignore exceptions for now by catching everything.
-- for now this exception probably arises because of duplicate insert.
-- fix this up later.
insertTrend :: Trend -> IO ()
insertTrend t =
    withDbRun $ DbSql.insert_ t `catch` (\(SomeException e) -> return ())

dbMigration :: IO ()
dbMigration = withDbRun $ runMigration $ migrate entityDefs $ entityDef
    (Nothing :: Maybe Trend)
