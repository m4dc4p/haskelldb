module Database.HaskellDB.HSQL.ODBC (
		  odbcConnect
		 , ODBCOptions(..)
		 ) where

import Database.HaskellDB.HSQL.Common
import qualified Database.HSQL.ODBC as ODBC (connect) 

data ODBCOptions = ODBCOptions { 
                               dsn :: String, -- ^ name binding in ODBC
                               uid :: String, -- ^ user id
                               pwd :: String  -- ^ password
                  	       }          

odbcConnect :: ODBCOptions -> (HSQL -> IO a) -> IO a
odbcConnect = 
    hsqlConnect (\opts -> ODBC.connect (dsn opts) (uid opts) (pwd opts))
