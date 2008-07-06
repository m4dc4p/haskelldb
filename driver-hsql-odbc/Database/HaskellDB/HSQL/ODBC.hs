-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HSQL.ODBC
-- Copyright   :  HWT Group 2003,
--                Bjorn Bringert 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------

module Database.HaskellDB.HSQL.ODBC (
		                     ODBCOptions(..),
		                     odbcConnect, 
                                     odbcDriverConnect,
                                     DriverInterface(..),
		                     driver
		                    ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HSQL
import Database.HaskellDB.DriverAPI
import Database.HaskellDB.Sql.Generate (SqlGenerator)
import qualified Database.HSQL.ODBC as ODBC (connect, driverConnect) 

data ODBCOptions = ODBCOptions { 
                               dsn :: String, -- ^ name binding in ODBC
                               uid :: String, -- ^ user id
                               pwd :: String  -- ^ password
                  	       }          

odbcConnect :: MonadIO m => SqlGenerator -> ODBCOptions -> (Database -> m a) -> m a
odbcConnect gen opts = 
    hsqlConnect gen (ODBC.connect (dsn opts) (uid opts) (pwd opts))

-- | DSN-less connection.
odbcDriverConnect :: MonadIO m => SqlGenerator -> String -> (Database -> m a) -> m a
odbcDriverConnect gen opts =
    hsqlConnect gen (ODBC.driverConnect opts)

options :: [(String, String)]
options =
    ("dsn", "Data Source Name") :
    ("uid", "User") :
    ("pwd", "Password") :
    []

odbcConnectOpts :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
odbcConnectOpts opts f = 
    do
    [a,b,c] <- getAnnotatedOptions options opts
    g <- getGenerator opts
    odbcConnect g (ODBCOptions {dsn = a,
                                uid = b,
			        pwd = c}) f

-- | This driver requires the following options: 
--   "dsn", "uid", "pwd"
driver :: DriverInterface
driver = defaultdriver { connect = odbcConnectOpts, requiredOptions = options }
