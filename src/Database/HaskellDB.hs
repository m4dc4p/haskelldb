-----------------------------------------------------------
-- |
-- Module      :  HaskellDB
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  non portable
--
-- HaskellDB is the main module that a user should 
-- import. Beside this module, a user should import a
-- particular database binding (ie. "Database.HaskellDB.HSQL.ODBC") 
-- and database definitions.
--
-- HaskellDB was originally written by Daan Leijen and it's 
-- design is described in the paper Domain Specific Embedded 
-- Compilers, Daan Leijen and Erik Meijer. 2nd USENIX 
-- Conference on Domain-Specific Languages (DSL), Austin, 
-- USA, October 1999 (<http://www.usenix.org/events/dsl99/>).
--
-----------------------------------------------------------
module Database.HaskellDB
	( Rel, Attr, Expr, Table, Query	-- abstract
	
	, ( # )
	, ( << )

	, (!)	
	, restrict, table, project
	, union, intersect, divide, minus
		
	, (.==.) , (.<>.), (.<.), (.<=.), (.>.), (.>=.)
	, (.&&.) , (.||.)
	, (.*.) , (./.), (.%.), (.+.), (.-.), (.++.)
	, _not, like, cat
	, isNull, notNull, fromNull
	, constant, constJust
	
	, count, _sum, _max, _min, avg
	, stddev, stddevP, variance, varianceP
	
	, asc, desc, order
	, top --, topPercent

        , _case
	, _default
	
	, Database				-- abstract
	, (!.)
	, query, lazyQuery, strictQuery
	, insert, delete, update, insertQuery
	, tables, describe, transaction
	
	, showQ, showOpt, showSql
	, genericConnect
	) where

import Database.HaskellDB.PrimQuery (ppPrimQuery)
import Database.HaskellDB.Sql       (toSql, ppSql)
import Database.HaskellDB.Optimize  (optimize)
import Database.HaskellDB.Query
import Database.HaskellDB.Database
import Database.HaskellDB.GenericConnect
import Text.PrettyPrint.HughesPJ (Doc) -- This is for the 
-- typesignatures of the show-functions

-----------------------------------------------------------
-- Show Queries, both as PrimQuery, Optimized PrimQuery and SQL
-----------------------------------------------------------
instance Show (Query (Rel r)) where
  showsPrec _ query = shows (showSql query)


showQ :: Query (Rel r) -> Doc
showQ = ppPrimQuery . runQuery

showOpt :: Query (Rel r) -> Doc
showOpt = ppPrimQuery . optimize . runQuery

showSql :: Query (Rel r) -> Doc
showSql = ppSql . toSql . optimize . runQuery 
