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
				      dynConnect_
                                    ) where

import Database.HaskellDB.Database (Database)
import Database.HaskellDB.DriverAPI
import Database.HaskellDB.Version

import System.Plugins (loadPackage,unloadPackage,resolveObjs,loadFunction_)
import System.Plugins.Utils (encode)

import Data.Char
import Data.List (isPrefixOf)

-- | Loads a function from a package module, given the package name,
--   module name and symbol name.
loadPackageFunction :: String -- ^ Package name, including version number.
                    -> String -- ^ Module name
                    -> String -- ^ Symbol to lookup in the module
                    -> IO (Maybe a)
loadPackageFunction pkgName moduleName functionName =
    do
    loadPackage pkgName
    resolveObjs (unloadPackage pkgName)
    loadFunction_ (encode moduleName) functionName

-- | Loads a driver by package and module name.
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

-- | Load a driver by a simple driver name.
dynConnect_ :: String -- ^ Driver, in a human readable format, for
                      -- example "odbc" or "mysql"
              -> [(String,String)] -- ^ Arguments to the driver 
              -> (Database -> IO a) -- ^ Database action to run
              -> IO a
dynConnect_ d opts f = 
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