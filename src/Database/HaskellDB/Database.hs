-----------------------------------------------------------
-- |
-- Module      :  Database
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines standard database operations and the
-- primitive hooks that a particular database binding
-- must provide.
--
-----------------------------------------------------------
module Database.HaskellDB.Database ( 
			-- * Operators
			(!.)
		-- * Type declarations
		, Row, rowSelect
		, Database(..)
		
		-- * Function declarations
		, query, lazyQuery, strictQuery
		, insert, delete, update, insertNew
		, tables, describe
		) where

import Database.HaskellDB.HDBRec
import Database.HaskellDB.FieldType
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Optimize (optimize)
import Database.HaskellDB.Query	(Rel(..), Attr, Table(..), Query, Expr(..)
		,runQuery, runQueryRel, exprs, labels)



infix 9 !. 

-- | The (!.) operator selects over returned records from
--  the database (= rows)

(!.) :: Row row a => row r -> Attr f r a -> a
row !. attr     = rowSelect attr row


-- | 'Row' and 'Database' abstract away over any particular
-- database. The 'Row' class enables selection over rows
-- that are returned from a database query. The 'Database'
-- data type contains all the primitive functions that
-- a particular database binding should provide.
-- Look in the 'Ado' module for an example of a database binding.
class Row row a where
  rowSelect :: Attr f r a -> row r -> a
  
data Database db row
	= Database  
	  { dbQuery  :: forall r. db -> PrimQuery -> Rel r -> IO [row r]
	  , dbInsert :: db -> TableName -> PrimQuery -> IO ()
  	  , dbInsertNew :: db -> TableName -> Assoc -> IO ()
  	  , dbDelete :: db -> TableName -> [PrimExpr] -> IO ()
  	  , dbUpdate :: db -> TableName -> [PrimExpr] -> Assoc -> IO ()
	  , dbTables :: db -> IO [TableName]
	  , dbDescribe :: db -> TableName -> IO [(Attribute,FieldDef)]
  	  , database :: db
  	  }
  	  
dbInvoke :: (Database db row -> db -> a) -> Database db row -> a
dbInvoke fun db		= (fun db) (database db)  	  

-----------------------------------------------------------
-- Database operations
-----------------------------------------------------------  	    	  

-- | performs a query on a database
query :: Database db row -> Query (Rel r) -> IO [(row r)]
query	= lazyQuery

-- | lazy query performs a lazy query on a database	  
lazyQuery :: Database db row -> Query (Rel r) -> IO [(row r)]
lazyQuery db q	
	= (dbInvoke dbQuery db) (optimize primQuery) (rel)
	where
	  (primQuery,rel) = runQueryRel q


-- | retrieves all the results directly in Haskell. This allows
-- a connection to close as early as possible.
strictQuery :: Database db row -> Query (Rel r) -> IO [(row r)]
strictQuery db q
        = do{ xs <- lazyQuery db q
            ; let xs' = seqList xs
            ; xs' `seq` return xs'
            } 
        where
	  seqList []      = []
	  seqList (x:xs)  = let xs' = seqList xs
                  	    in  xs' `seq` x:xs'
	
-- | inserts a bunch of records
insert :: ShowRecRow r => Database db row -> Table r -> Query (Rel r) -> IO ()
insert db (Table name assoc) q
	= (dbInvoke dbInsert db) name (optimize (runQuery q))

-- | inserts a new single record
insertNew :: ShowRecRow r => Database db row -> Table r -> HDBRec r -> IO ()
insertNew db (Table name assoc) newrec	
	= (dbInvoke dbInsertNew db) name (zip (attrs assoc) (exprs newrec))
	where
	  attrs   = map (\(attr,AttrExpr name) -> name)
	  
-- | deletes a bunch of records	  
delete :: ShowRecRow r => Database db row -> Table r -> (Rel r -> Expr Bool) -> IO ()
delete db (Table name assoc) criteria
	= (dbInvoke dbDelete db) name [substAttr assoc primExpr]
	where
	  (Expr primExpr)  = criteria rel
	  rel		   = Rel 0 (map fst assoc)
	  
	  
-- | updates a bunch of records	  
update :: (ShowRecRow s,ShowRecRow r) => Database db row -> Table r -> 
		(Rel r -> Expr Bool) -> (Rel r -> HDBRec s) -> IO ()
update db (Table name assoc) criteria assignFun
	= (dbInvoke dbUpdate db) name [substAttr assoc primExpr] newassoc			     
	where
	  (Expr primExpr)= criteria rel
	  	
	  newassoc	= zip (map subst (labels assigns))
	  		      (exprs assigns)
	  		      
	  subst label	= case (lookup label assoc) of
	  		    (Just (AttrExpr name)) -> name
	  		    (Nothing)		   -> error ("Database.update: attribute '" 
	  		    				     ++ label ++ "' is not in database '" ++ name ++ "'")
	  	
	  assigns	= assignFun rel
	  rel		= Rel 0 (map fst assoc)
	
-- | List all tables in the database
tables :: Database db row -- ^ Database
       -> IO [TableName] -- ^ Names of all tables in the database
tables db = dbInvoke dbTables db

-- | List all columns in a table, along with their types
describe :: Database db row -- ^ Database
	 -> TableName -- ^ Name of the tables whose columns are to be listed
	 -> IO [(Attribute,FieldDef)] -- ^ Name and type info for each column
describe db = dbInvoke dbDescribe db
