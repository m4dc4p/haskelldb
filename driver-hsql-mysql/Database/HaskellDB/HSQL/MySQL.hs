-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HSQL.MySQL
-- Copyright   :  HWT Group 2003,
--                Bjorn Bringert 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------
module Database.HaskellDB.HSQL.MySQL (MySQLOptions(..), mysqlConnect,
                                      DriverInterface(..), driver) where

import Database.HaskellDB.Database
import Database.HaskellDB.HSQL
import Database.HaskellDB.DriverAPI
import Database.HaskellDB.Sql.MySQL (generator)
import qualified Database.HSQL.MySQL as MySQL (connect) 

data MySQLOptions = MySQLOptions { 
                                  server :: String, -- ^ server name
                                  db :: String,     -- ^ database name
                                  uid :: String,    -- ^ user id
                                  pwd :: String     -- ^ password
                                 }

mysqlConnect :: MonadIO m => MySQLOptions -> (Database -> m a) -> m a
mysqlConnect opts = 
    hsqlConnect generator (MySQL.connect (server opts) (db opts) (uid opts) (pwd opts))

options :: [(String, String)]
options =
    ("server", "Server") :
    ("db", "Database") :
    ("uid", "User") :
    ("pwd", "Password") :
    []

mysqlConnectOpts :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
mysqlConnectOpts opts f = 
    do
    [a,b,c,d] <- getAnnotatedOptions options opts
    mysqlConnect (MySQLOptions {server = a, db = b,
                                uid = c, pwd = d}) f

-- | This driver requires the following options: 
--   "server", "db", "uid", "pwd"
driver :: DriverInterface
driver = defaultdriver { connect = mysqlConnectOpts, requiredOptions = options }
