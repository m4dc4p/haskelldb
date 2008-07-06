-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HSQL.SQLite
-- Copyright   :  HWT Group 2003,
--                Bjorn Bringert 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Interface to SQLite 2 <http://www.hwaci.com/sw/sqlite/>
-- databases.
--
-----------------------------------------------------------
module Database.HaskellDB.HSQL.SQLite (
		   SQLiteOptions(..), sqliteConnect,
                   DriverInterface(..), driver
		  ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HSQL
import Database.HaskellDB.DriverAPI
import Database.HaskellDB.Sql.SQLite as SQLite

import qualified Database.HSQL.SQLite2 as SQLite2 (connect) 
import System.IO

data SQLiteOptions = SQLiteOptions { 
				    filepath :: FilePath, -- ^ database file
				    mode :: IOMode        -- ^ access mode
                  		   }

sqliteConnect :: MonadIO m => SQLiteOptions -> (Database -> m a) -> m a
sqliteConnect opts = 
    hsqlConnect SQLite.generator (SQLite2.connect (filepath opts) (mode opts))

options :: [(String, String)]
options =
    ("filepath", "File path") :
    ("mode", "r/w/a/rw for read/(over)write/append/random access") :
    []

sqliteConnectOpts :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
sqliteConnectOpts opts f = 
    do
    [a,b] <- getAnnotatedOptions options opts
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

-- | This driver requires the following options: 
--   "filepath", "mode"
-- The possible values of the "mode" option are "r", "w", "rw"
driver :: DriverInterface
driver = defaultdriver {connect = sqliteConnectOpts, requiredOptions = options}
