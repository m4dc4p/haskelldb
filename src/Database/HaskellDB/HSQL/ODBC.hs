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
--
-- $Revision: 1.11 $
-----------------------------------------------------------

module Database.HaskellDB.HSQL.ODBC (
		  odbcConnect, 
                  odbcDriverConnect,
		  ODBCOptions(..),
		  driver
		 ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HSQL.Common
import Database.HaskellDB.DriverAPI
import qualified Database.HSQL.ODBC as ODBC (connect, driverConnect) 

data ODBCOptions = ODBCOptions { 
                               dsn :: String, -- ^ name binding in ODBC
                               uid :: String, -- ^ user id
                               pwd :: String  -- ^ password
                  	       }          

odbcConnect :: MonadIO m => ODBCOptions -> (Database -> m a) -> m a
odbcConnect = 
    hsqlConnect (\opts -> ODBC.connect (dsn opts) (uid opts) (pwd opts))

odbcDriverConnect :: MonadIO m => String -> (Database -> m a) -> m a
odbcDriverConnect =
    hsqlConnect (\opts -> ODBC.driverConnect opts)

odbcConnectOpts :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
odbcConnectOpts opts f = 
    do
    [a,b,c] <- getOptions ["dsn","uid","pwd"] opts
    odbcConnect (ODBCOptions {dsn = a,
                              uid = b,
			      pwd = c}) f

driver :: DriverInterface
driver = defaultdriver {connect = odbcConnectOpts}
