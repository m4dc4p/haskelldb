-----------------------------------------------------------
-- |
-- Module      :  DynConnect
-- Copyright   :  Anders Hockersten (c), chucky@dtek.chalmers.se
-- License     :  BSD-style
--
-- Maintainer  :  chucky@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This contains functions for loading drivers dynamically 
-- and connecting to databases using them.
-----------------------------------------------------------

module Database.HaskellDB.DynConnect (
				      dynConnect,
				      dynConnectPWD
                                    ) where

import Database.HaskellDB.Database (Database)
import Database.HaskellDB.DriverAPI
import Database.HaskellDB.Version

import System.Plugins

import Data.Char
import Data.List (isPrefixOf)

-- | Loads a given driver and connects using it
dynConnect :: String -- ^ Driver package
           -> String -- ^ Driver module
	   -> [(String,String)] -- ^ Options to the driver
	   -> (Database -> IO a) -- ^ Database action to run
	   -> IO a
dynConnect p m opts f = 
    do
    res <- loadPackageFunction p m "driver"
    v <- case res of
		  Nothing -> fail $ "Couldn't load " ++ m ++ ".driver"
                                    ++ " from package " ++ p
                  Just v -> return v
    connect v opts f

-- | Provided as a helper function for connecting to the standard drivers
-- note that this REQUIRES that the driver is available in the same directory
-- as the program being run
dynConnectPWD :: String -- ^ Driver, in a human readable format, for
                        -- example "odbc" or "mysql"
              -> [(String,String)] -- ^ Arguments to the driver 
              -> (Database -> IO a) -- ^ Database action to run
              -> IO a
dynConnectPWD d opts f = 
    case map toLower d of
         "odbc"                       -> c "hsql-odbc" "HSQL.ODBC"
         "mysql"                      -> c "hsql-mysql" "HSQL.MySQL"
         "sqlite"                     -> c "hsql-sqlite" "HSQL.SQLite"
         x | "postgre" `isPrefixOf` x -> c "hsql-postgresql" "HSQL.PostgreSQL"
         x | "wx" `isPrefixOf` x      -> c "wx" "WX"
         _            -> fail $ "DynConnect: unknown driver: " ++ d
  where c p m = dynConnect p' m' opts f
         where p' = "haskelldb-" ++ p ++ "-" ++ version
               m' = "Database.HaskellDB." ++ m