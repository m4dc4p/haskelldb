-----------------------------------------------------------
-- Daan Leijen (c) 1999, daan@cs.uu.nl
--
-- HaskellDB is the main module that a user should 
-- import. Beside this module, a user should import a
-- particular database binding (ie. Ado) and database
-- definitions (ie. Pubs).
-----------------------------------------------------------
module HaskellDB 
	( Rel, Attr, Expr, Table, Query	-- abstract
	
	, ( # )


	, (!)	
	, restrict, table
	, union, intersect, divide, minus
		
	, (.==.) , (.<>.), (.<.), (.<=.), (.>.), (.>=.)
	, (.&&.) , (.||.)
	, (.*.) , (./.), (.%.), (.+.), (.-.), (.++.)
	, _not, like, cat
	, isNull, notNull
	, constant
	
	, count, _sum, _max, _min, avg
	, stddev, stddevP, variance, varianceP
	
	, asc, desc, order
	, top, topPercent
	
	, Row, Database				-- abstract
	, (!.)
	, query, lazyQuery, strictQuery
	, insert, delete, update, insertNew
	, tables, describe
	
	, showQ, showOpt, showSql
	
	) where

import PrimQuery (ppPrimQuery)
import Sql       (toSql, ppSql)
import Optimize  (optimize)
import Query
import Database
import HDBRecUtils

-----------------------------------------------------------
-- Show Queries, both as PrimQuery, Optimized PrimQuery and SQL
-----------------------------------------------------------
instance Show (Query (Rel r)) where
  showsPrec d query     = shows (showSql query)
  
showQ       = ppPrimQuery . runQuery
showOpt     = ppPrimQuery . optimize . runQuery
showSql     = ppSql . toSql . optimize . runQuery 
