-----------------------------------------------------------
-- |
-- Module      :  DBSpecToDatabase
-- Copyright   :  HWT Group (c) 2004, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Connects to a database and generates stuff in it according
-- to what's inside the DBSpec
-----------------------------------------------------------
module Database.HaskellDB.DBSpec.DBSpecToDatabase 
    (dbSpecToDatabase)
    where

import Database.HaskellDB.DBSpec

-- FIXME: Write code for this

dbSpecToDatabase :: DBInfo -> IO ()
dbSpecToDatabase dbinfo = undefined $ constructNonClashingDBInfo dbinfo
