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
                      DriverInterface(..),
		      driver
		      ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HDBC
import Database.HaskellDB.DriverAPI
import Database.HaskellDB.Sql.PostgreSQL

import Database.HDBC.PostgreSQL (connectPostgreSQL)

postgresqlConnect :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
postgresqlConnect opts = hdbcConnect generator (connectPostgreSQL conninfo)
    where conninfo = unwords [ k ++ "=" ++ v | (k,v) <- opts]

-- | This driver passes its options through to HDBC.
-- HDBC refers to
-- <http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT>
-- for the meaning of the options.
driver :: DriverInterface
driver = defaultdriver { connect = postgresqlConnect
                       , requiredOptions = [] }
