module HSQL_ODBC (
		  odbcConnect
		 , ODBCOptions(..)
		 ) where

import Database
import HSQL_driver
import qualified Database.HSQL.ODBC as ODBC (connect) 

data ODBCOptions = ODBCOptions { 
                               dsn :: String, -- ^ name binding in ODBC
                               uid :: String, -- ^ user id
                               pwd :: String  -- ^ password
                  	       }          

odbcConnect :: ODBCOptions -> (HSQL -> IO a) -> IO a
odbcConnect opts action = 
    do
    conn <- ODBC.connect (dsn opts) (uid opts) (pwd opts)
    hsqlAction conn action
