-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.ODBC.PostgreSQL
-- Copyright   :  HWT Group (c) 2003, Bjorn Bringert (c) 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------
module Database.HaskellDB.HDBC.ODBC (
		      odbcConnect,
		      driver
		      ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HDBC.Common
import Database.HaskellDB.DriverAPI
import Database.HDBC.ODBC (connectODBC)

odbcConnect :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
odbcConnect opts = hdbcConnect connectODBC conninfo
    where conninfo = unwords [ k ++ "=" ++ v | (k,v) <- opts]

driver :: DriverInterface
driver = defaultdriver {connect = odbcConnect}
