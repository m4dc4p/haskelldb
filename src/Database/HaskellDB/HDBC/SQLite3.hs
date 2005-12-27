-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HDBC.SQLite3
-- Copyright   :  HWT Group (c) 2003, Bjorn Bringert (c) 2005
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
		   sqliteConnect,
		   driver
		  ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HDBC.Common
import Database.HaskellDB.DriverAPI
import Database.HDBC.Sqlite3 (connectSqlite3) 
import System.IO

data SQLiteOptions = SQLiteOptions { 
				    filepath :: FilePath
                  		   }

sqliteConnect :: MonadIO m => FilePath -> (Database -> m a) -> m a
sqliteConnect path = hdbcConnect connectSqlite3 path

sqliteConnectOpts :: [(String,String)] -> (Database -> IO a) -> IO a
sqliteConnectOpts opts f = 
    do
    [a] <- getOptions ["filepath"] opts
    sqliteConnect a f

driver = defaultdriver {connect = sqliteConnectOpts}
