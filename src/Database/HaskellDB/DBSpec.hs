-----------------------------------------------------------
-- |
-- Module      :  DBSpec
-- Copyright   :  HWT Group (c) 2004, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- DBSpec is the new and improved way of specifying databases.
-- It is designed to be able to describe a database in such a 
-- way that it can easily be converted to a DBDirect-spec OR
-- directly into a database
--
-- 
-----------------------------------------------------------

module Database.HaskellDB.DBSpec 
    (DBInfo(..),TInfo(..),CInfo(..),DBOptions(..),
     FieldDesc, FieldType(..),
     makeDBSpec,makeTInfo,
     makeCInfo,constructNonClashingDBInfo,ppDBInfo,ppTInfo,ppCInfo,
     ppDBOptions,dbInfoToDoc,finalizeSpec,dbToDBSpec,dbSpecToDatabase)
    where

import Database.HaskellDB.FieldType
import Database.HaskellDB.DBSpec.DBInfo
import Database.HaskellDB.DBSpec.DatabaseToDBSpec
import Database.HaskellDB.DBSpec.DBSpecToDatabase
