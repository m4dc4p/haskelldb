-----------------------------------------------------------
-- Daan Leijen (c) 1999, daan@cs.uu.nl
--
-- module Database.
-- 	defines standard database operations and the
--	primitive hooks that a particular database binding
--	must provide.
--
-- "(lazy)query" performs a (lazy) query on a database
-- "strictQuery" retrieves all the results directly in Haskell. This allows
--		a connection to close as early as possible.
-- "insert"	inserts a bunch of records
-- "insertNew"  inserts a new single record
-- "delete"	deletes a bunch of records
-- "update"	updates a bunch of records
-----------------------------------------------------------
module Database ( (!.)
		
		, Row, rowSelect
		, Database(..)
		
		, query, lazyQuery, strictQuery
		, insert, delete, update, insertNew
		) where

import HDBRec
import FieldType
import PrimQuery
import Optimize (optimize)
import Query	(Rel(..), Attr, Table(..), Query, Expr(..)
		,runQuery, runQueryRel, exprs, labels)

-----------------------------------------------------------
-- The (!.) operator selects over returned records from
-- the database (= rows)
-----------------------------------------------------------
infix 9 !. 

(!.) :: Row row a => row r -> Attr r a -> a
row !. attr     = rowSelect attr row

-----------------------------------------------------------
-- 'Row' and 'Database' abstract away over any particular
-- database. The 'Row' class enables selection over rows
-- that are returned from a database query. The 'Database'
-- data type contains all the primitive functions that
-- a particular database binding should provide.
-- Look in the 'Ado' module for an example of a database binding.
-----------------------------------------------------------
class Row row a where
  rowSelect :: Attr r a -> row r -> a
  
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
  	  
  	  
dbInvoke fun db		= (fun db) (database db)  	  

-----------------------------------------------------------
-- Database operations
-----------------------------------------------------------  	    	  

query,lazyQuery,strictQuery :: Database db row -> Query (Rel r) -> IO [(row r)]

query	= lazyQuery

lazyQuery db q	
	= (dbInvoke dbQuery db) (optimize primQuery) (rel)
	where
	  (primQuery,rel) = runQueryRel q
	  

strictQuery db q
        = do{ xs <- lazyQuery db q
            ; let xs' = seqList xs
            ; xs' `seq` return xs'
            } 
        where
	  seqList []      = []
	  seqList (x:xs)  = let xs' = seqList xs
                  	    in  xs' `seq` x:xs'
	
	
	
-----------------------------------------------------------
-- 
-----------------------------------------------------------

insert :: ShowRecRow r => Database db row -> Table r -> Query (Rel r) -> IO ()
insert db (Table name assoc) q
	= (dbInvoke dbInsert db) name (optimize (runQuery q))

insertNew :: ShowRecRow r => Database db row -> Table r -> HDBRec r -> IO ()
insertNew db (Table name assoc) newrec	
	= (dbInvoke dbInsertNew db) name (zip (attrs assoc) (exprs newrec))
	where
	  attrs   = map (\(attr,AttrExpr name) -> name)
	  
delete :: ShowRecRow r => Database db row -> Table r -> (Rel r -> Expr Bool) -> IO ()
delete db (Table name assoc) criteria
	= (dbInvoke dbDelete db) name [substAttr assoc primExpr]
	where
	  (Expr primExpr)  = criteria rel
	  rel		   = Rel 0 (map fst assoc)
	  
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
	

tables :: Database db row -> IO [TableName]
tables db = dbInvoke dbTables db

describe :: Database db row -> TableName -> IO [(Attribute,FieldDef)]
describe db = dbInvoke dbDescribe db