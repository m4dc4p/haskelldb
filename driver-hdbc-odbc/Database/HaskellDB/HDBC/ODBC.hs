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
import Database.HaskellDB.Sql.Generate (SqlGenerator)

import Database.HDBC.ODBC (connectODBC)

odbcConnect :: MonadIO m => SqlGenerator -> [(String,String)] -> (Database -> m a) -> m a
odbcConnect gen opts = hdbcConnect gen (connectODBC conninfo)
    -- strangely enough, mysql+unixodbc want a semicolon terminating connstring
    where conninfo = foldr (\(k,v) z -> k ++ "=" ++ v ++ ";" ++ z) [] opts

options :: [(String, String)]
options =
    []

odbcConnectOpts :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
odbcConnectOpts opts f = 
    do gen <- getGenerator opts
       let opts' = filter ((/="generator") . fst) opts
       odbcConnect gen opts' f

-- | This driver passes its options through to HDBC.
driver :: DriverInterface
driver = defaultdriver { connect = odbcConnectOpts, requiredOptions = options }
