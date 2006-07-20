-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HSQL.PostgreSQL
-- Copyright   :  HWT Group 2003,
--                Bjorn Bringert 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------
module Database.HaskellDB.HSQL.PostgreSQL (
		      PostgreSQLOptions(..), postgresqlConnect,
                      DriverInterface(..), driver
		      ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HSQL
import Database.HaskellDB.DriverAPI
import Database.HaskellDB.Sql.PostgreSQL
import qualified Database.HSQL.PostgreSQL as PostgreSQL (connect) 

data PostgreSQLOptions = PostgreSQLOptions { 
				  server :: String, -- ^ server name
				  db :: String,     -- ^ database name
				  uid :: String,    -- ^ user id
				  pwd :: String     -- ^ password
                  		 }

postgresqlConnect :: MonadIO m => PostgreSQLOptions -> (Database -> m a) -> m a
postgresqlConnect opts = 
    hsqlConnect generator (PostgreSQL.connect (server opts) (db opts) (uid opts) (pwd opts))

postgresqlConnectOpts :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
postgresqlConnectOpts opts f = 
    do
    [a,b,c,d] <- getOptions ["server","db","uid","pwd"] opts
    postgresqlConnect (PostgreSQLOptions {server = a, db = b,
                                          uid = c, pwd = d}) f

-- | This driver requires the following options: 
--   "server", "db", "uid", "pwd"
driver :: DriverInterface
driver = defaultdriver { connect = postgresqlConnectOpts }
