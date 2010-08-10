-----------------------------------------------------------
-- |
-- Module      :  DBInfo
-- Copyright   :  HWT Group (c) 2004, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This is the "core" file of the DBSpec files. It defines
-- a DBInfo and important functions on it.
--
-- 
-----------------------------------------------------------

module Database.HaskellDB.DBSpec.DBInfo
    (DBInfo(..),TInfo(..),CInfo(..),DBOptions(..),makeDBSpec,
     makeTInfo,makeCInfo,ppDBInfo,ppTInfo,ppCInfo,ppDBOptions,
     dbInfoToDoc,finalizeSpec,constructNonClashingDBInfo)
    where

import qualified Database.HaskellDB.DBSpec.PPHelpers as PP
import Database.HaskellDB.FieldType (FieldDesc, FieldType(BStrT, StringT), )
import Data.Char (toLower, isAlpha)
import Text.PrettyPrint.HughesPJ

-- | Defines a database layout, top level
data DBInfo = DBInfo {dbname :: String -- ^ The name of the database
		     ,opts :: DBOptions  -- ^ Any options (i.e whether to use
					 --   Bounded Strings)
		     ,tbls :: [TInfo]    -- ^ Tables this database contains
		     }
	      deriving (Show)

data TInfo = TInfo {tname :: String  -- ^ The name of the table
		   ,cols :: [CInfo]  -- ^ The columns in this table
		   }
	      deriving (Eq,Show)
data CInfo = CInfo {cname :: String    -- ^ The name of this column
		   ,descr :: FieldDesc -- ^ The description of this column
		   }
	     deriving (Eq,Show)

data DBOptions = DBOptions
		   {useBString :: Bool -- ^ Use Bounded Strings?
		   ,makeIdent  :: PP.MakeIdentifiers -- ^ Conversion routines from Database identifiers to Haskell identifiers
		   }

instance Show DBOptions where
   showsPrec p opts =
      showString "DBOptions {useBString = " .
      shows (useBString opts) .
      showString "}"


-- | Creates a valid declaration of a DBInfo. The variable name will be the
--   same as the database name
dbInfoToDoc :: DBInfo -> Doc
dbInfoToDoc dbi@(DBInfo {dbname = n
                        , opts = opt}) 
    = fixedName <+> text ":: DBInfo"
      $$ fixedName <+> equals <+> ppDBInfo dbi
      where fixedName = text . PP.identifier (makeIdent opt) $ n

-- | Pretty prints a DBInfo
ppDBInfo :: DBInfo -> Doc
ppDBInfo (DBInfo {dbname=n, opts=o, tbls = t}) 
    = text "DBInfo" <+> 
	 braces (vcat (punctuate comma (
		 text "dbname =" <+> doubleQuotes (text n) :
		 text "opts =" <+> ppDBOptions o :
		 text "tbls =" <+> 
		 brackets (vcat (punctuate comma (map ppTInfo t))) : [])))

ppTInfo :: TInfo -> Doc
ppTInfo (TInfo {tname=n,cols=c})
    = text "TInfo" <+> 
      braces (vcat (punctuate comma (
		 text "tname =" <+> doubleQuotes (text n) :
		 text "cols =" <+> 
		 brackets (vcat (punctuate comma (map ppCInfo c))) : [])))

ppCInfo :: CInfo -> Doc
ppCInfo (CInfo {cname=n,descr=(val,null)})
    = text "CInfo" <+>
      braces (vcat (punctuate comma (
	         text "cname =" <+> doubleQuotes (text n) :
		 text "descr =" <+> 
		 parens (text (show val) <> comma <+> text (show null)) : [])))

ppDBOptions :: DBOptions -> Doc
ppDBOptions (DBOptions {useBString = b})
    = text "DBOptions" <+>
      braces (text "useBString =" <+> text (show b))

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
    let db' = makeDBNameUnique dbinfo
    in  if equalObjectNames db' (makeDBNameUnique db')
          then db'
	  else constructNonClashingDBInfo db'

equalObjectNames :: DBInfo -> DBInfo -> Bool
equalObjectNames db1 db2 =
   dbname db1 == dbname db2 &&
   tbls db1 == tbls db2

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
