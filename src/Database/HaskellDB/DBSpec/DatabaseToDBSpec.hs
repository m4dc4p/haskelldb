-----------------------------------------------------------
-- |
-- Module      :  DatabaseToDBSpec
-- Copyright   :  HWT Group (c) 2004, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Connects to a Database and generates a DBSpec specification
-- from it
-----------------------------------------------------------
module Database.HaskellDB.DBSpec.DatabaseToDBSpec
    (dbToDBSpec)
    where

import Database.HaskellDB
import Database.HaskellDB.FieldType
import Database.HaskellDB.DBSpec.DBInfo

-- | Connects to a database and generates a specification from it
dbToDBSpec :: Bool -- ^ Use bounded strings?
	   -> String  -- ^ the name our database should have
	   -> Database -- ^ the database connection
	   -> IO DBInfo    -- ^ return a DBInfo
dbToDBSpec useBStr name dbconn
    = do ts <- tables dbconn
	 descs <- mapM (describe dbconn) ts
         let cinfos = map (map $ uncurry makeCInfo) descs
	 let tinfos = map (uncurry makeTInfo) (zip ts cinfos)
	 return $ makeDBSpec name (DBOptions {useBString = useBStr}) tinfos
