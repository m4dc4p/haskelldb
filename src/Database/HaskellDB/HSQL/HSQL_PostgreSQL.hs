module HSQL_PostgreSQL (
		      PostgreSQLOptions(..)
		      , postgresqlConnect
		      ) where

import HSQL_driver
import qualified Database.HSQL.PostgreSQL as PostgreSQL (connect) 

data PostgreSQLOptions = PostgreSQLOptions { 
				  server :: String, -- ^ server name
				  db :: String,     -- ^ database name
				  uid :: String,    -- ^ user id
				  pwd :: String     -- ^ password
                  		 }

postgresqlConnect :: PostgreSQLOptions -> (HSQL -> IO a) -> IO a
postgresqlConnect = 
    hsqlConnect (\opts -> PostgreSQL.connect 
		            (server opts) (db opts) (uid opts) (pwd opts))
