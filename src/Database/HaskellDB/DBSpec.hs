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

import Database.HaskellDB.DBSpec.DatabaseToDBSpec
import Database.HaskellDB.DBSpec.DBSpecToDatabase

import Data.Char
import Text.PrettyPrint.HughesPJ

-- | Defines a database layout, top level
data DBInfo = DBInfo {dbname :: String, -- ^ The name of the database
		      opts :: DBOptions, -- ^ Any options (i.e whether to use
					 --   Bounded Strings)
		      tbls :: [TInfo]    -- ^ Tables this database contains
		     }
	      deriving (Eq,Show)

data TInfo = TInfo {tname :: String, -- ^ The name of the table
		    cols :: [CInfo]  -- ^ The columns in this table
		   }
	      deriving (Eq,Show)
data CInfo = CInfo {cname :: String, -- ^ The name of this column
		    descr :: FieldDesc -- ^ The description of this column
		   }
	     deriving (Eq,Show)

data DBOptions = DBOptions {useBString :: Bool -- ^ Use Bounded Strings?
			   }
		 deriving (Eq,Show)

-- | Creates a valid declaration of a DBInfo. The variable name will be the
--   same as the database name
dbInfoToDoc :: DBInfo -> Doc
dbInfoToDoc dbi@(DBInfo {dbname=n}) 
    = text n <+> text ":: DBInfo"
      $$ text n <+> text "=" <+> text (show dbi)

-- | Does a final "touching up" of a DBInfo before it is used by i.e DBDirect.
-- This converts any Bounded Strings to ordinary strings if that flag is set.
finalizeSpec :: DBInfo -> DBInfo
finalizeSpec dbi = if (useBString (opts dbi)) then 
		   dbi else stripBStr dbi

-- | Converts all BStrings to ordinary Strings
stripBStr :: DBInfo -> DBInfo
stripBStr dbi = fixTables dbi
    where
    fixTables dbi = dbi{tbls=map fixCols (tbls dbi)}
    fixCols tbl = tbl{cols=map oneCol (cols tbl)}
    oneCol col = col{descr = fixDescr (descr col)}
    fixDescr col = case fst col of
		       BStrT _ -> (StringT,snd col)
		       _       -> col

-- | Creates a DBInfo
makeDBSpec :: String -- ^ The name of the Database
	   -> DBOptions -- ^ Options
	   -> [TInfo]  -- ^ Tables
	   -> DBInfo -- ^ The generated DBInfo
makeDBSpec name opt tinfos
    = DBInfo {dbname = name, opts = opt, tbls = tinfos}

-- | Creates a TInfo
makeTInfo :: String -- ^ The table name
	  -> [CInfo] -- ^ Columns 
	  -> TInfo -- ^ The generated TInfo
makeTInfo name cinfs
    = TInfo {tname = name, cols = cinfs}

-- | Creates a CInfo
makeCInfo :: String -- ^ The column name
	  -> FieldDesc -- ^ What the column contains
	  -> CInfo -- ^ The generated CInfo
makeCInfo name fdef
    = CInfo {cname = name, descr = fdef}


-----------------------------------------------------
-- Functions for avoiding nameclashes
-----------------------------------------------------

-- | Constructs a DBInfo that doesn't cause nameclashes
constructNonClashingDBInfo :: DBInfo -> DBInfo
constructNonClashingDBInfo dbinfo = 
    let db' = makeDBNameUnique dbinfo in 
	if db' == makeDBNameUnique db' then db'
	   else constructNonClashingDBInfo db'

-- | Makes a table name unique among all other table names
makeTblNamesUnique :: [TInfo] -> [TInfo]
makeTblNamesUnique [] = []
makeTblNamesUnique (t:[]) = t:[]
makeTblNamesUnique (t:tt:ts)
    | compNames (tname t) (tname tt) 
	= t: (makeTblNamesUnique ((tblNewName tt) : ts))
    | True = t : makeTblNamesUnique (tt:ts)
    where 
    tblNewName tinfo@TInfo{tname=n} = tinfo{tname=newName (Left n)}

-- | Makes a field name unique among all other field names
makeFieldNamesUnique :: [CInfo] -> [CInfo]
makeFieldNamesUnique [] = []
makeFieldNamesUnique (f:[]) = f:[]
makeFieldNamesUnique (f:ff:fs)
    | compNames (cname f) (cname ff) 
	= f: (makeFieldNamesUnique ((fNewName ff) :fs))
    | True = f : makeFieldNamesUnique (ff:fs)
    where 
    fNewName cinfo@CInfo{cname=n} = cinfo{cname=newName (Right n)}


-- | makes the dbname unique in a database
makeDBNameUnique :: DBInfo -> DBInfo
makeDBNameUnique dbinfo 
    = dbinfo{tbls=map (makeTblNameUnique (dbname dbinfo)) (tbls dbinfo)}

-- | makes a supplied name unique in a table and its subfields
makeTblNameUnique :: String -> TInfo -> TInfo
makeTblNameUnique s tinfo
    | compNames s (tname tinfo) = 
	tinfo{cols=map (makeFieldNameUnique s) 
	      (cols tinfo{tname=newName (Left (tname tinfo))})}
    | True = tinfo{cols=map (makeFieldNameUnique s) (cols tinfo)}

-- | makes a supplied name unique in a field
makeFieldNameUnique :: String -> CInfo -> CInfo
makeFieldNameUnique s cinfo 
    | compNames s (cname cinfo) = cinfo{cname=newName (Right (cname cinfo))}
    | True = cinfo

-- | Gives a String a new name, according to its type
newName :: Either String String -- ^ Either a Table or a Field
	-> String -- ^ The new name
newName (Left t) = t ++ "T"
newName (Right n) = n ++ "F"

-- | Case insensitive String comparison (there probably is a standard function
--   for this, there ought to be anyway
compNames :: String -> String -> Bool
compNames s1 s2 = map toLower s1 == map toLower s2
