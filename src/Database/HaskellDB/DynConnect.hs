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
				      dynGenericConnect
                                    ) where

import Data.Char
import Database.HaskellDB.Database(Database)
import Database.HaskellDB.DriverAPI
import Plugins

-- | Loads a given driver and connects using it
dynConnect :: String -- ^ Driver name and path
	   -> [String] -- ^ Options to the driver
	   -> (Database -> IO a) -- ^ Database to work on
	   -> IO a
dynConnect dnam opts db = do
			  res <- load dnam ["."] [] "driver"
			  v <- case res of
				   LoadFailure e -> error (unlines e)
				   LoadSuccess _ v -> return v
			  v opts db

-- | Provided as a helper function for connecting to the standard drivers
-- note that this REQUIRES that the driver is available in the same directory
-- as the program being run
dynGenericConnect :: String -- ^ Driver, in a human readable format, for
                            -- example "odbc" or "mysql"
                  -> [String] -- ^ Arguments to the driver 
                  -> ((Database -> IO a) -> IO a)
dynGenericConnect a b = dynGenericConnect' (map toLower a) b

dynGenericConnect' :: String -> [String] -> ((Database -> IO a) -> IO a)
dynGenericConnect' "odbc" opts 
    = dynConnect "ODBC.o" opts
dynGenericConnect' "mysql" opts
    = dynConnect "MySQL.o" opts
dynGenericConnect' "sqlite" opts
    = dynConnect "SQLite.o" opts
dynGenericConnect' "postgre" opts
    = dynConnect "PostgreSQL.o" opts
dynGenericConnect' "postgresql" opts
    = dynConnect "PostgreSQL.o" opts
dynGenericConnect' "wx" opts
    = dynConnect "WX.o" opts
dynGenericConnect' "wxhaskell" opts
    = dynConnect "WX.o" opts
dynGenericConnect' _ _ 
    = error "dynGenericConnect failed to find a match"