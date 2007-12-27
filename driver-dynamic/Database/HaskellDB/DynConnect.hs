-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.DynConnect
-- Copyright   :  Anders Hockersten 2004, chucky@dtek.chalmers.se
--                Bjorn Bringert 2005-2006
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
                                      driver,
				      dynConnect,
				      dynConnect_
                                    ) where

import Database.HaskellDB.Database (Database)
import Database.HaskellDB.DriverAPI
import Database.HaskellDB.Version

import System.Plugins (loadPackageFunction)

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char
import Data.List (isPrefixOf)

driver :: DriverInterface
driver = defaultdriver {
           connect = \opts f -> do [driver] <- getOptions ["driver"] opts
                                   dynConnect_ driver opts f
         }

-- | Loads a driver by package and module name.
dynConnect :: MonadIO m => 
              String -- ^ Driver package
           -> String -- ^ Driver module
	   -> [(String,String)] -- ^ Options to the driver
	   -> (Database -> m a) -- ^ Database action to run
	   -> m a
dynConnect p m opts f = 
    do
    res <- liftIO $ loadPackageFunction p m "driver"
    v <- case res of
		  Nothing -> fail $ "Couldn't load " ++ m ++ ".driver"
                                    ++ " from package " ++ p
                  Just v -> return v
    connect v opts f

-- | Load a driver by a simple driver name corresponding to the
--   package suffix
dynConnect_ :: MonadIO m => 
               String -- ^ Driver package suffix, e.g. "WX", "HSQL.MySQL",
                      -- "HDBC.PostgreSQL"
            -> [(String,String)] -- ^ Arguments to the driver 
            -> (Database -> m a) -- ^ Database action to run
            -> m a
dynConnect_ d opts f = 
    dynConnect p m opts f
    where p = "haskelldb-" ++ (map c d) ++ "-" ++ version
          m = "Database.HaskellDB." ++ d
          c x = if x == '.' then '-' else toLower x
