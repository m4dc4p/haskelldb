-----------------------------------------------------------
-- |
-- Module      :  DBSpec
-- Copyright   :  HWT Group (c) 2004, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- DBSpec is the new and improved way of specifying databases.
-- It is designed to be able to describe a database in such a 
-- way that it can easily be converted to a DBDirect-spec OR
-- directly into a database
-----------------------------------------------------------

module Database.HaskellDB.DBSpec 
    (DBInfo(..),TInfo(..),CInfo(..),DBOptions(..),makeDBSpec,makeTInfo,
     makeCInfo,constructNonClashingDBInfo,dbInfoToDoc,finalizeSpec,
     dbToDBSpec,dbSpecToDatabase)
    where

import Database.HaskellDB.FieldType
import Database.HaskellDB.DBSpec.DBInfo
import Database.HaskellDB.DBSpec.DatabaseToDBSpec
import Database.HaskellDB.DBSpec.DBSpecToDatabase