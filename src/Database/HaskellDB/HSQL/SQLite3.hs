-----------------------------------------------------------
-- |
-- Module      :  HaskellDB
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Interface to the SQLite <http://www.hwaci.com/sw/sqlite/>
-- databases.
--
-- $Revision: 1.6 $
-----------------------------------------------------------
module Database.HaskellDB.HSQL.SQLite3 (
		   SQLiteOptions(..),
		   sqliteConnect,
		   driver
		  ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HSQL.Common
import Database.HaskellDB.DriverAPI
import qualified Database.HSQL.SQLite3 as SQLite3 (connect) 
import System.IO

data SQLiteOptions = SQLiteOptions { 
				    filepath :: FilePath, -- ^ database file
				    mode :: IOMode        -- ^ access mode
                  		   }

sqliteConnect :: MonadIO m => SQLiteOptions -> (Database -> m a) -> m a
sqliteConnect = 
    hsqlConnect (\opts -> SQLite3.connect 
		            (filepath opts) (mode opts))

sqliteConnectOpts :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
sqliteConnectOpts opts f = 
    do
    [a,b] <- getOptions ["filepath","mode"] opts
    m <- readIOMode b
    sqliteConnect (SQLiteOptions {filepath = a,
				  mode = m}) f

readIOMode :: Monad m => String -> m IOMode
readIOMode s = 
    case s of
           "r" -> return ReadMode
           "w" -> return WriteMode
           "a" -> return AppendMode
           "rw" -> return ReadWriteMode
           _ -> case reads s of
                             [(x,"")] -> return x
                             _ -> fail $ "Bad IO mode: " ++ s

driver :: DriverInterface
driver = defaultdriver {connect = sqliteConnectOpts}
