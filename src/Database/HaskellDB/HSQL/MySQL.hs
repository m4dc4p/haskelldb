module Database.HaskellDB.HSQL.MySQL (
		   MySQLOptions(..)
		  , mysqlConnect
		  ) where

import Database.HaskellDB.HSQL.Common
import qualified Database.HSQL.MySQL as MySQL (connect) 

data MySQLOptions = MySQLOptions { 
				  server :: String, -- ^ server name
				  db :: String,     -- ^ database name
				  uid :: String,    -- ^ user id
				  pwd :: String     -- ^ password
                  		 }

mysqlConnect :: MySQLOptions -> (HSQL -> IO a) -> IO a
mysqlConnect = 
    hsqlConnect (\opts -> MySQL.connect 
		            (server opts) (db opts) (uid opts) (pwd opts))
