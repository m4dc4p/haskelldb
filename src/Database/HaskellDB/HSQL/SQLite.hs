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
-----------------------------------------------------------
module Database.HaskellDB.HSQL.SQLite (
		   SQLiteOptions(..)
		  , sqliteConnect
		  ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HSQL.Common
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
