{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
-----------------------------------------------------------
-- |
-- Module      :  DriverAPI
-- Copyright   :  Anders Hockersten (c), chucky@dtek.chalmers.se
-- License     :  BSD-style
--
-- Maintainer  :  chucky@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- This exports an API that all drivers must conform to. It
-- is used by the end user to load drivers either dynamically
-- or statically.
-----------------------------------------------------------

module Database.HaskellDB.DriverAPI (
				     DriverInterface(..),
                                     MonadIO, 
				     defaultdriver,
                                     getOptions,
                                     getAnnotatedOptions,
                                     getGenerator
				    ) where

import Database.HaskellDB.Database (Database)

import Database.HaskellDB.Sql.Generate (SqlGenerator)
import Database.HaskellDB.Sql.Default (defaultSqlGenerator)
import Database.HaskellDB.Sql.MySQL as MySQL
import Database.HaskellDB.Sql.PostgreSQL as PostgreSQL
import Database.HaskellDB.Sql.SQLite as SQLite


import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO)

-- | Interface which drivers should implement.
--   The 'connect' function takes some driver specific name, value pairs
--   use to setup the database connection, and a database action to run.
--   'requiredOptions' lists all required options with a short description,
--   that is printed as help in the DBDirect program.
data DriverInterface = DriverInterface
    { connect :: forall m a. MonadIO m => [(String,String)] -> (Database -> m a) -> m a,
      requiredOptions :: [(String, String)]
    }

-- | Default dummy driver, real drivers should overload this
defaultdriver :: DriverInterface 
defaultdriver =
    DriverInterface {
        connect = error "DriverAPI.connect: not implemented",
        requiredOptions = error "DriverAPI.requiredOptions: not implemented"}

-- | Can be used by drivers to get option values from the given
--   list of name, value pairs.
getOptions ::Monad m => [String] -- ^ names of options to get
           -> [(String,String)] -- ^ options given
           -> m [String] -- ^ a list of the same length as the first argument
                         --   with the values of each option. Fails in the given
                         --   monad if any options is not found.
getOptions [] _ = return []
getOptions (x:xs) ys = 
    case lookup x ys of
                     Nothing -> fail $ "Missing field " ++ x
                     Just v -> liftM (v:) $ getOptions xs ys

-- | Can be used by drivers to get option values from the given
--   list of name, value pairs.
--   It is intended for use with the 'requiredOptions' value of the driver.
getAnnotatedOptions :: Monad m =>
              [(String,String)] -- ^ names and descriptions of options to get
           -> [(String,String)] -- ^ options given
           -> m [String] -- ^ a list of the same length as the first argument
                         --   with the values of each option. Fails in the given
                         --   monad if any options is not found.
getAnnotatedOptions opts = getOptions (map fst opts)

-- | Gets an 'SqlGenerator' from the "generator" option in the given list.
--   Currently available generators: "mysql", "postgresql", "sqlite", "default"
getGenerator :: Monad m => 
                [(String,String)] -- ^ options given
           -> m SqlGenerator -- ^ An SQL generator. If there was no
                             --   "generator" option, the default is used.
                             -- Fails if the generator is unknown
getGenerator opts = maybe (return defaultSqlGenerator) f $ lookup "generator" opts
    where f n = maybe (fail msg) return $ lookup n generators
              where msg = "Unknown SqlGenerator: " ++ n

generators :: [(String,SqlGenerator)]
generators = [("mysql",      MySQL.generator),
              ("postgresql", PostgreSQL.generator),
              ("sqlite",     SQLite.generator),
              ("default",    defaultSqlGenerator)]
