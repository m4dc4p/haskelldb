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

import Database.HaskellDB
import Database.HaskellDB.FieldType
import Database.HaskellDB.DBSpec
import Data.List

-- | Prints a TInfo into an SQL statement
tInfoToSql :: TInfo -> String
tInfoToSql (TInfo {tname=n,cols=cs}) = "CREATE TABLE " ++ n ++ " ( " 
			++ concat (intersperse "," 
				   (map cInfoToSql cs))
			++ ")"

-- | Prints a CInfo into an SQL statement
cInfoToSql :: CInfo -> String
cInfoToSql (CInfo {cname=n, descr=(ft,nullable)}) 
    = n ++ " " ++ (sshow ft) ++	(if not nullable then " not null" else "")

-- | Converts a DBInfo to a real life Database
dbSpecToDatabase :: DBInfo -- ^ The DBInfo to generate from
		 -> ((Database -> IO a) -> IO a) -- ^ A function that connects
						 -- to a Database
		 -> IO ()
dbSpecToDatabase dbfunc dbinfo 
    = return () --dbfunc (return ()) -- här borde stoppas in något som tar en Database som sista argument