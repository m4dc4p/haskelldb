module HSQL_PostgeSQL (
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
postgresqlConnect opts action = 
    do
    conn <- PostgreSQL.connect (server opts) (db opts) (uid opts) (pwd opts)
    hsqlAction conn action