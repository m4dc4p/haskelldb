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
-- or statically
-----------------------------------------------------------
module Database.HaskellDB.DriverAPI (
				     DriverInterface(..),
				     defaultdriver
				    ) where

import Database.HaskellDB.Database (Database)

data DriverInterface = DriverInterface
    {
     connect :: forall a. [String] -- ^ Driver specific options
             -> (Database -> IO a) -- ^ Database to work on
             -> IO a
    }

-- | Default dummy driver, real drivers should overload this
defaultdriver :: DriverInterface 
defaultdriver = DriverInterface {connect = undefined}
