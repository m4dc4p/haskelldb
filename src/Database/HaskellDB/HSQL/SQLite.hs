-----------------------------------------------------------
-- |
-- Module      :  HaskellDB
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Interface to the SQLite <http://www.hwaci.com/sw/sqlite/>
-- databases.
--
-- $Revision: 1.4 $
-----------------------------------------------------------
module Database.HaskellDB.HSQL.SQLite (
		   SQLiteOptions(..),
		   sqliteConnect,
		   driver
		  ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HSQL.Common
import Database.HaskellDB.DriverAPI
import qualified Database.HSQL.SQLite as SQLite (connect) 
import System.IO

data SQLiteOptions = SQLiteOptions { 
				    filepath :: FilePath, -- ^ database file
				    mode :: IOMode        -- ^ access mode
                  		   }

sqliteConnect :: SQLiteOptions -> (Database -> IO a) -> IO a
sqliteConnect = 
    hsqlConnect (\opts -> SQLite.connect 
		            (filepath opts) (mode opts))

sqliteFlatConnect :: [String] -> (Database -> IO a) -> IO a
sqliteFlatConnect (a:b:[]) = sqliteConnect (SQLiteOptions {filepath = a,
							   mode = read b})
sqliteFlatConnect _ = error "sqliteFlatConnect failed: Invalid number of arguments"

driver = defaultdriver {connect = sqliteFlatConnect}
