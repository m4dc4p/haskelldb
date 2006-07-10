-----------------------------------------------------------
-- |
-- Module      :  SQL
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- Transform a PrimQuery (relational expression) to SQL
-- and pretty print SQL
--
-- $Revision: 1.23 $
-----------------------------------------------------------
module Database.HaskellDB.Sql ( 
		-- * Type Declarations
	     SqlSelect(..) 
	   , SqlUpdate(..) 
	   , SqlDelete(..) 
	   , SqlInsert(..)
	   -- * Function Declarations
	   , toSql, ppSql
	   , toUpdate, ppUpdate
	   , toDelete, ppDelete
	   , toInsert, ppInsert
	   , toInsertQuery
	   , toCreateDB, toCreateTable, ppCreate
	   , toDropDB, toDropTable, ppDrop
	   ) where

import Data.List (intersect)
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.FieldType

import Text.PrettyPrint.HughesPJ

-----------------------------------------------------------
-- SQL data type
-----------------------------------------------------------

-- | Data type representing the SQL SELECT statement.
-- Can be created with the 'toSql' function', and pretty printed
-- with the 'ppSql' function.
data SqlSelect  = SqlSelect   { options  :: [String]
			      , attrs    :: [(Attribute,String)]
                              , tables   :: [(TableName,SqlSelect)]
                              , criteria :: [String]
                              , groupby  :: [String]
                              , orderby	 :: [PrimExpr]
			      , limit    :: [String]
                              }
                | SqlBin   String SqlSelect SqlSelect
                | SqlTable TableName -- ^ Select a whole table named TableName.
                | SqlEmpty -- ^ Empty select.


-- | Data type representing the SQL UPDATE statement.
-- Can be created with the 'toUpdate' function, and pretty printed with
-- the 'ppUpdate' function.
data SqlUpdate  = SqlUpdate TableName [String] [String] -- ^ Update 
-- the rows in table TableName that corresponds to the criterias
-- supplid with the second field with values from the last field.


-- | Data type representing the SQL DELETE statement.
-- Can be created with the 'toDelete' function, and pretty printed with
-- the 'ppDelete' function.
data SqlDelete  = SqlDelete TableName [String] -- ^ Delete the rows 
-- in the table TableName that corresponds to the criterias 
-- supplied as a list of strings in the last field.

-- | Data type representing the SQL INSERT statement.
data SqlInsert  = SqlInsert  TableName [(Attribute,String)]
                | SqlInsertQuery TableName SqlSelect

-- | Data type representing the SQL CREATE statement.
data SqlCreate = SqlCreateDB String -- ^ Create a database
		   | SqlCreateTable TableName [(Attribute,FieldDesc)] 
-- ^ Create a table with name TableName and with a list of Attributes and 
-- FieldDescs.

-- | Data type representing the SQL DROP statement.
data SqlDrop = SqlDropDB String -- ^ Delete a database
	     | SqlDropTable TableName -- ^ Delete a table named TableName

newSelect :: SqlSelect
newSelect       = SqlSelect { options   = []
			    , attrs 	= []
			    , tables 	= []
			    , criteria 	= []
			    , groupby	= []
			    , orderby	= []
			    , limit     = []}


-----------------------------------------------------------
-- SELECT
-- Hmm, bit messy.
-----------------------------------------------------------

-- invariant: null attrs => null groupby

-- | Creates a 'SqlSelect' based on the 'PrimQuery' supplied.
-- Corresponds to the SQL statement SELECT.
toSql   :: PrimQuery -> SqlSelect
toSql   = foldPrimQuery (empty,table,project,restrict,binary,special)
        where
          empty             	= SqlEmpty
          table name schema 	= SqlTable name
	 
          project assoc q
          	| hasAggr    = select { groupby = map toSqlExpr nonAggrs }
          	| otherwise  = select 
                where
                  select   = sql { attrs = toSqlAssoc assoc }
                  sql      = toSelect q

                  hasAggr  = any isAggregate exprs
                  
                  -- TODO: we should make sure that every non-aggregate 
                  -- is only a simple attribute expression
                  nonAggrs = filter (not.isAggregate) exprs
                  
                  exprs    = map snd assoc
		  
          restrict expr q
                = sql { criteria = toSqlExpr expr : criteria sql }
                where
                  sql   = toSelect q
                  
          -- binary assumes that q1 and q2 are not empty
          binary Times q1 q2  
          	| null (attrs q1) = addTable q1 q2
          	| null (attrs q2) = addTable q2 q1
          	| otherwise       = newSelect { tables = [("",q1),("",q2)] }
          	where
          	  addTable sql q  = sql{ tables = tables sql ++ [("",q)] }
		 
          binary op q1 q2         
          	= SqlBin (toSqlOp op) q1 q2


	  special (Order newOrder) q
	  	= sql { orderby = newOrder ++ oldOrder }  
		where
		  sql 	    = toSelect q

		  -- all ordering expressions only in the old order
		  oldOrder  = filter notdup (orderby sql)
		  -- order expressions can only be Asc t.f or Desc t.f,
		  -- thus attrInExpr will give [f] for every expression
		  notdup x  = disjoint (attrInExpr x) (attrInOrder newOrder)
		  disjoint x y = null (x `intersect` y)
		  	    
		  	    
          special op@(Top _ _) q
          	= sql { limit = show (ppSpecialOp op) : limit sql }
          	where
                  sql	    = toSelect q


toSelect :: SqlSelect -> SqlSelect
toSelect sql    = case sql of
                    (SqlEmpty)          -> newSelect
                    (SqlTable name)     -> newSelect { tables = [("",sql)] }
                    (SqlBin op q1 q2)   -> newSelect { tables = [("",sql)] }
                    s | null (attrs s) -> sql
                      | otherwise  -> newSelect { tables = [("",sql)] }

toSqlAssoc :: Assoc -> [(Attribute,String)]
toSqlAssoc      = map (\(attr,expr) -> (attr, toSqlExpr expr))

toSqlExpr :: PrimExpr -> String
toSqlExpr       = show . ppPrimExpr

toSqlOp :: RelOp -> String
toSqlOp Union        = "UNION"
toSqlOp Intersect    = "INTERSECT"
toSqlOp Divide       = "DIVIDE"
toSqlOp Difference   = "EXCEPT"


-----------------------------------------------------------
-- SELECT, show & pretty print
-----------------------------------------------------------

-- | Pretty prints a 'SqlSelect'
ppSql :: SqlSelect -> Doc
ppSql (SqlSelect options attrs tables criteria groupby orderby limit)
    = text "SELECT DISTINCT" <+> (hsep . map text) options <+> ppAttrs attrs
      $$ f "FROM " ppTables tables
      $$ f "WHERE" ppCriteria criteria
      $$ f "GROUP BY" ppGroupBy groupby
      $$ f "ORDER BY" ppOrderBy orderby
      $$ (hsep . map text) limit
    where
    f clause action xs    | null xs       = empty
			  | otherwise     = text clause <+> action xs

ppSql (SqlBin op sql1 sql2)     = parens (ppSql sql1) 
				  $$ text op 
				  $$ parens (ppSql sql2)
ppSql (SqlTable name)           = text name
ppSql (SqlEmpty)                = text ""


-- helpers

ppAttrs :: [(Attribute,String)] -> Doc
ppAttrs []	= text "*"
ppAttrs xs      = vcat $ punctuate comma (map nameAs xs)

ppCriteria :: [String] -> Doc
ppCriteria      = vcat . punctuate (text " AND ") . map text

-- FIXME: table aliases start from 1 in every select, which means that
-- with binary RelOps we can get table alias clashes.
ppTables :: [(TableName,SqlSelect)] -> Doc
ppTables        = vcat . punctuate comma . map ppTable . 
		  zipWith tableAlias [1..]

ppGroupBy :: [String] -> Doc
ppGroupBy	= vcat . punctuate comma  . map text

ppOrderBy :: [PrimExpr] -> Doc
ppOrderBy ord	= ppSpecialOp (Order ord)

tableAlias :: Int -> (TableName,SqlSelect) -> (TableName,SqlSelect)
tableAlias i (_,sql)  		= ("T" ++ show i,sql)

ppTable :: (TableName,SqlSelect) -> Doc
ppTable (alias,(SqlTable name)) = ppAs alias (text name)
ppTable (alias,sql)             = ppAs alias (parens (ppSql sql))

-- | Print a name-value binding, or just the name if
--   name and value are the same.
nameAs :: (Attribute,String) -> Doc
nameAs (name,expr) | name == expr  = text name
                   | otherwise     = ppAs name (text expr)              

ppAs :: TableName -> Doc -> Doc
ppAs alias expr    | null alias    = expr                               
                   | otherwise     = expr <+> (hsep . map text) ["as",alias]

-----------------------------------------------------------
-- INSERT
-----------------------------------------------------------

-- | Creates a 'SqlInsert'. Corresponds to the SQL statement
-- INSERT INTO which is used to insert new rows in a table.
toInsertQuery :: TableName -- ^ Name of the table
	      -> PrimQuery -- ^ What to insert
	      -> SqlInsert
toInsertQuery table qtree
	= SqlInsertQuery table (toSql qtree)

-- | Creates a 'SqlInsert'. Almost the same as to InsertQuery
-- except that it take a 'Assoc' instead of a 'PrimQuery'
-- as second argument.
toInsert :: TableName -- ^ Name of the table
	 -> Assoc -- ^ What to insert.
	 -> SqlInsert
toInsert table assoc
	= SqlInsert table (map showExpr assoc)
	where
	  showExpr (attr,expr)	= (attr,show (ppPrimExpr expr))

ppInsert :: SqlInsert -> Doc
ppInsert (SqlInsertQuery table select)
	= text "INSERT INTO" <+> text table
        $$ ppSql select

ppInsert (SqlInsert table exprs)
    = text "INSERT INTO" <+> text table 
      <+> parens (vcat $ punctuate comma (map text names))
      $$ text "VALUES"   <+> parens (vcat $ punctuate comma (map text values))
    where
    (names,values)        = unzip exprs


-----------------------------------------------------------
-- DELETE
-----------------------------------------------------------
-- | Creates a 'SqlDelete'. Corresponds to the SQL statement
-- DELETE which deletes rows in a table.
toDelete :: TableName -- ^ Name of the table
	 -> [PrimExpr] -- ^ Conditions which must all be true for a row
                       --   to be deleted.
	 -> SqlDelete
toDelete name exprs
        = SqlDelete name (map toSqlExpr exprs)

ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete name exprs) =
    text "DELETE FROM" <+> text name $$ f "WHERE" ppCriteria exprs
   where
     f clause action xs | null xs    = empty
                        | otherwise  = text clause <+> action xs

-----------------------------------------------------------
-- UPDATE
-----------------------------------------------------------

-- | Creates a 'SqlUpdate'. Corresponds to the SQL statement
-- UPDATE which updates data in a table.
toUpdate :: TableName -- ^ Name of the table to update.
	 -> [PrimExpr]  -- ^ Conditions which must all be true for a row
                        --   to be updated.
	 -> Assoc -- ^ Update the data with this.
	 -> SqlUpdate
toUpdate name criteria assigns
        = SqlUpdate name cs (map showAssign assigns)
        where
          cs = map toSqlExpr criteria
          showAssign (attr,expr)
          	= attr ++ " = " ++ toSqlExpr expr

-- | Pretty prints a 'SqlUpdate'
ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate name criteria assigns)
        = text "UPDATE" <+> text name
        $$ text "SET" <+> (vcat $ punctuate comma (map text assigns))
        $$ f "WHERE" ppCriteria criteria
        where
           f clause action xs   | null xs    = empty
                                | otherwise  = text clause <+> action xs

-----------------------------------------------------------
-- CREATE
-----------------------------------------------------------

-- | Use this to create a 'SqlCreate' data type corresponding to 
-- the SQL statement CREATE DATABASE which creates a new database.
toCreateDB :: String -- ^ name of the database.
	   -> SqlCreate
toCreateDB name = SqlCreateDB name

-- | Use this to create a 'SqlCreate' data type corresponding to 
-- the SQL statement CREATE which creates a new table.
toCreateTable :: TableName -- ^ name of the table to be created.
	      -> [(Attribute,FieldDesc)] -- ^ List of Attributes and FiledDescs 
					 -- that describes the table.
	      -> SqlCreate
toCreateTable name xs = SqlCreateTable name xs

-- | Pretty prints a 'SqlCreate'. 
ppCreate :: SqlCreate -> Doc
ppCreate (SqlCreateDB name) = text "CREATE DATABASE" <+> text name
ppCreate (SqlCreateTable name xs) 
    = text "CREATE TABLE" <+> text name 
      <+> parens (vcat $ punctuate comma (map ppF xs))
    where
    ppF (fname,(ftype,nullable)) 
	= text fname <+> text (sshow ftype)
	  <> if nullable then text "" else text " not null"

-----------------------------------------------------------
-- DROP
-----------------------------------------------------------

-- | Creates a 'SqlDrop' that delete the database with the 
-- name given as the first argument.
toDropDB :: String -> SqlDrop
toDropDB name = SqlDropDB name

-- | Creates a 'SqlDrop' that delete the database named
-- in the first argument.
toDropTable :: TableName -> SqlDrop
toDropTable name = SqlDropTable name


-- | Pretty prints a 'SqlDrop'.
ppDrop :: SqlDrop -> Doc
ppDrop (SqlDropDB name) = text "DROP DATABASE" <+> text name
ppDrop (SqlDropTable name) 
    = text "DROP TABLE" <+> text name 
