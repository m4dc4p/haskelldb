-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HDBC.PostgreSQL
-- Copyright   :  HWT Group (c) 2003, Bjorn Bringert (c) 2005
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------
module Database.HaskellDB.HDBC.PostgreSQL (
		      postgresqlConnect,
		      driver
		      ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HDBC.Common
import Database.HaskellDB.DriverAPI
import Database.HDBC.PostgreSQL (connectPostgreSQL)

postgresqlConnect :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
postgresqlConnect opts = hdbcConnect connectPostgreSQL conninfo
    where conninfo = unwords [ k ++ "=" ++ v | (k,v) <- opts]

driver :: DriverInterface
driver = defaultdriver {connect = postgresqlConnect}
