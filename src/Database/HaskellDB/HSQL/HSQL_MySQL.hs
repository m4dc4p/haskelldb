module HSQL_MySQL (
		   MySQLOptions(..)
		  , mysqlConnect
		  ) where

import Database
import HSQL_driver
import qualified Database.HSQL.MySQL as MySQL (connect) 

data MySQLOptions = MySQLOptions { 
				  server :: String, -- ^ server name
				  db :: String,     -- ^ database name
				  uid :: String,    -- ^ user id
				  pwd :: String     -- ^ password
                  		 }

mysqlConnect :: MySQLOptions -> (HSQL -> IO a) -> IO a
mysqlConnect opts action = 
    do
    conn <- MySQL.connect (server opts) (db opts) (uid opts) (pwd opts)
    hsqlAction conn action