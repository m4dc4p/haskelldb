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
--
-- $Revision: 1.9 $
-----------------------------------------------------------

module Database.HaskellDB.HSQL.ODBC (
		  odbcConnect, 
		  ODBCOptions(..),
		  driver
		 ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HSQL.Common
import Database.HaskellDB.DriverAPI
import qualified Database.HSQL.ODBC as ODBC (connect) 

data ODBCOptions = ODBCOptions { 
                               dsn :: String, -- ^ name binding in ODBC
                               uid :: String, -- ^ user id
                               pwd :: String  -- ^ password
                  	       }          

odbcConnect :: ODBCOptions -> (Database -> IO a) -> IO a
odbcConnect = 
    hsqlConnect (\opts -> ODBC.connect (dsn opts) (uid opts) (pwd opts))

odbcFlatConnect :: [String] -> (Database -> IO a) -> IO a
odbcFlatConnect (a:b:c:[]) = odbcConnect (ODBCOptions {dsn = a,
						       uid = b,
						       pwd = c})
odbcFlatConnect _ = error "odbcFlatConnect failed: Invalid number of arguments"

driver = defaultdriver {connect = odbcFlatConnect}