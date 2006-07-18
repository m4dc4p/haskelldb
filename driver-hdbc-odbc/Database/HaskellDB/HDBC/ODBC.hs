-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HDBC.ODBC
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
                      DriverInterface(..), driver
		      ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HDBC
import Database.HaskellDB.DriverAPI
import Database.HDBC.ODBC (connectODBC)

odbcConnect :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
odbcConnect opts = hdbcConnect (connectODBC conninfo)
    where conninfo = unwords [ k ++ "=" ++ v | (k,v) <- opts]

-- | This driver passes its options through to HDBC.
driver :: DriverInterface
driver = defaultdriver { connect = odbcConnect }
