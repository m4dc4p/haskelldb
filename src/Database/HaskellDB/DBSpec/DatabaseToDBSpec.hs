-----------------------------------------------------------
-- |
-- Module      :  DatabaseToDBSpec
-- Copyright   :  HWT Group (c) 2004, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  portable
--
-- Connects to a Database and generates a DBSpec specification
-- from it.
--
-- 
-----------------------------------------------------------
module Database.HaskellDB.DBSpec.DatabaseToDBSpec
    (dbToDBSpec)
    where

import Database.HaskellDB.Database (Database, tables, describe, )
import Database.HaskellDB.DBSpec.DBInfo
   (DBInfo, makeCInfo, makeTInfo, makeDBSpec, 
    DBOptions(DBOptions), useBString, makeIdent, )

import qualified Database.HaskellDB.DBSpec.PPHelpers as PP


-- | Connects to a database and generates a specification from it
dbToDBSpec :: Bool -- ^ Use bounded strings?
           -> PP.MakeIdentifiers -- ^ style of generated Haskell identifiers, cOLUMN_NAME vs. columnName
	   -> String  -- ^ the name our database should have
	   -> Database -- ^ the database connection
	   -> IO DBInfo    -- ^ return a DBInfo
dbToDBSpec useBStr mkIdent name dbconn
    = do ts <- tables dbconn
	 descs <- mapM (describe dbconn) ts
         let cinfos = map (map $ uncurry makeCInfo) descs
	 let tinfos = map (uncurry makeTInfo) (zip ts cinfos)
	 return $ makeDBSpec name (DBOptions {useBString = useBStr, makeIdent = mkIdent }) tinfos
