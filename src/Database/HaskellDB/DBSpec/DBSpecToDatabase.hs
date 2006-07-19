-----------------------------------------------------------
-- |
-- Module      :  DBSpecToDatabase
-- Copyright   :  HWT Group (c) 2004, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Connects to a database and generates stuff in it according
-- to what's inside the DBSpec.
--
-- 
-----------------------------------------------------------
module Database.HaskellDB.DBSpec.DBSpecToDatabase 
    (dbSpecToDatabase,tInfoToTable)
    where

import Database.HaskellDB.Database
import Database.HaskellDB.FieldType
import Database.HaskellDB.DBSpec.DBInfo

-- | Converts a DBInfo to a real life Database, note that the database must
-- exist for this to work
dbSpecToDatabase :: Database -- ^ A Database
		 -> DBInfo -- ^ The DBInfo to generate from
		 -> IO ()
dbSpecToDatabase db = mapM_ (tInfoToTable db) . tbls

-- | Create a database table specified by a 'TInfo'.
tInfoToTable :: Database -> TInfo -> IO ()
tInfoToTable db t = createTable db (tname t) (tInfoCols t)

tInfoCols :: TInfo -> [(String,FieldDesc)] 
tInfoCols t = [(cname c, descr c) | c <- cols t, cname c /= ""]
