-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HSQL.Oracle
-- Copyright   :  HWT Group 2003,
--                Bjorn Bringert 2006,
--                Henning Thielemann 2008
-- License     :  BSD-style
--
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------

module Database.HaskellDB.HSQL.Oracle (
                                     OracleOptions(..),
                                     oracleConnect,
                                     DriverInterface(..),
                                     driver
                                    ) where

import Database.HaskellDB.Database
import Database.HaskellDB.HSQL
import Database.HaskellDB.DriverAPI
import Database.HaskellDB.Sql.Generate (SqlGenerator)
import qualified Database.HSQL.Oracle as Oracle
import qualified Database.HSQL        as HSQL
import Database.HSQL.Types (connTables)
import Control.Exception (bracket)


data OracleOptions =
   OracleOptions {
      dsn :: String, -- ^ name binding in Oracle
      uid :: String, -- ^ user id
      pwd :: String  -- ^ password
   }

oracleConnect ::
   MonadIO m =>
   SqlGenerator -> OracleOptions -> (Database -> m a) -> m a
oracleConnect gen opts =
    hsqlConnect gen (Oracle.connect (dsn opts) (uid opts) (pwd opts))
{-
HSQL's connTables asks for COREDB_SYSTEM,
but newer Oracle versions (e.g. 10g) do not have it.
Instead they provide a USER_TABLES.
This should clearly be fixed in HSQL not here.

    let connector = Oracle.connect (dsn opts) (uid opts) (pwd opts)
        getTables =
           bracket connector HSQL.disconnect $ \c ->
--              inTransaction c $ \c ->
                retrieveTables c
    in  hsqlConnect gen
           (fmap (\c -> c {connTables = getTables}) connector)

retrieveTables :: HSQL.Connection -> IO [String]
retrieveTables c =
   HSQL.query c "select table_name from user_tables" >>=
      HSQL.collectRows (flip HSQL.getFieldValue "TABLE_NAME")
-}

options :: [(String, String)]
options =
    ("dsn", "Data Source Name") :
    ("uid", "User") :
    ("pwd", "Password") :
    []

oracleConnectOpts ::
   MonadIO m =>
   [(String,String)] -> (Database -> m a) -> m a
oracleConnectOpts opts f =
    do
    [a,b,c] <- getAnnotatedOptions options opts
    g <- getGenerator opts
    oracleConnect g
       (OracleOptions
          {dsn = a,
           uid = b,
           pwd = c}) f

-- | This driver requires the following options:
--   "dsn", "uid", "pwd"
driver :: DriverInterface
driver = defaultdriver { connect = oracleConnectOpts, requiredOptions = options }
