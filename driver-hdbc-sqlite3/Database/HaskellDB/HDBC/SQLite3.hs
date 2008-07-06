-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HDBC.SQLite3
-- Copyright   :  HWT Group 2003, 
--                Bjorn Bringert 2005-2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Interface to the HDBC sqlite3 back-end.
--
-----------------------------------------------------------
module Database.HaskellDB.HDBC.SQLite3 (
		   SQLiteOptions(..), sqliteConnect,
                   DriverInterface(..), driver
		  ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HDBC
import Database.HaskellDB.DriverAPI
import Database.HaskellDB.Sql.SQLite as SQLite
import Database.HDBC.Sqlite3 (connectSqlite3) 

import System.IO

data SQLiteOptions = SQLiteOptions { 
				    filepath :: FilePath
                  		   }

sqliteConnect :: MonadIO m => FilePath -> (Database -> m a) -> m a
sqliteConnect path = hdbcConnect SQLite.generator (connectSqlite3 path)

options :: [(String, String)]
options =
    ("filepath", "File path") :
    []

sqliteConnectOpts :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
sqliteConnectOpts opts f = 
    do
    [a] <- getAnnotatedOptions options opts
    sqliteConnect a f

-- | This driver requires the following options: 
--   "filepath"
driver :: DriverInterface
driver = defaultdriver {connect = sqliteConnectOpts, requiredOptions = options}
