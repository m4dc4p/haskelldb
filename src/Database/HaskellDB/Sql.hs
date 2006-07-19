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
-- 
-----------------------------------------------------------
module Database.HaskellDB.Sql ( 
		-- * Type Declarations
	     SqlSelect(..) 
	   , SqlUpdate(..) 
	   , SqlDelete(..) 
	   , SqlInsert(..)
	   , SqlCreate(..) 
	   , SqlDrop(..)
	   -- * Function Declarations
	   , toSql
	   , toUpdate
	   , toDelete
	   , toInsert
	   , toInsertQuery
	   , toCreateDB, toCreateTable
	   , toDropDB, toDropTable
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

