{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------
-- |
-- Module      :  HaskellDB
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  "Justin Bailey" <jgbailey@gmail.com>
-- Stability   :  experimental
-- Portability :  non portable
--
-- HaskellDB is a Haskell library for expressing database queries and
-- operations in a type safe and declarative way. HaskellDB compiles a
-- relational algebra-like syntax into SQL, submits the operations to the
-- database for processing, and returns the results as ordinary Haskell
-- values.
--
-- This is the main module that the user should 
-- import. Beside this module, the user should import a
-- particular database binding (ie. "Database.HaskellDB.HSQL.ODBC") 
-- and database definitions.
--
-- HaskellDB was originally written by Daan Leijen and it's 
-- design is described in the paper Domain Specific Embedded 
-- Compilers, Daan Leijen and Erik Meijer. 2nd USENIX 
-- Conference on Domain-Specific Languages (DSL), Austin, 
-- USA, October 1999 (<http://www.usenix.org/events/dsl99/>).
--
-- This new version of HaskellDB was produced as a student project at
-- Chalmers University of Technology in Gothenburg, Sweden. The aim of the
-- project was to make HaskellDB a practically useful database library.
-- That work is described in 
-- Student Paper: HaskellDB Improved, 
-- Björn Bringert, Anders Höckersten, Conny Andersson, Martin Andersson, 
-- Mary Bergman, Victor Blomqvist and Torbjörn Martin. 
-- In Proceedings of the ACM SIGPLAN 2004 Haskell Workshop, Snowbird, Utah, 
-- USA, September 22, 2004.
-- (<http://haskelldb.sourceforge.net/haskelldb.pdf>)
--
-----------------------------------------------------------
module Database.HaskellDB
        ( module Data.HList
	, Rel, Expr, Table, Query, OrderExpr
        -- * Relational operators
	, restrict, table, project, unique
	, union, intersect, divide, minus
        , copy, copyAll, subQuery

  -- * Relational operators
  , restrict, table, project, unique
  , union, intersect, divide, minus
  , copy, copyAll, subQuery

  -- * Query expressions
  , (.==.) , (.<>.), (.<.), (.<=.), (.>.), (.>=.)
  , (.&&.) , (.||.)
  , (.*.) , (./.), (.+.), (.-.), (.%.), (.++.)
  , _not, like, _in, cat, _length
  , isNull, notNull, fromNull, fromVal
  , constant, constVal, constNull, constExpr
  , param, namedParam, Args, func
  , queryParams, Param, cast, coerce
  , literal, toStr
  , count, _sum, _max, _min, avg
  , stddev, stddevP, variance, varianceP
  , asc, desc, order
  , top , _case , _default

  -- * Database operations
  , Database				-- abstract
  , query, recCat
  , insert, delete, update, insertQuery
  , tables, describe, transaction
          
  -- * Debugging
  , showQuery, showQueryUnOpt, showSql, showSqlUnOpt
  ) where

import Database.HaskellDB.HDBRec

-- PrimQuery type is imported so that haddock can find it.
import Database.HaskellDB.PrimQuery (PrimQuery)
import Database.HaskellDB.Sql (SqlSelect(SqlSelect, SqlBin), SqlExpr(..), SqlName, Mark(..))
import qualified Database.HaskellDB.Sql as S (SqlSelect(..))
import Database.HaskellDB.Sql.Generate (sqlQuery)
import Database.HaskellDB.Sql.Default (defaultSqlGenerator)
import Database.HaskellDB.Sql.Print (ppSql)
import Database.HaskellDB.Optimize  (optimize)
import Database.HaskellDB.Query
import Database.HaskellDB.Database
import Text.PrettyPrint.HughesPJ (Doc)
import Data.Foldable (foldr')

-- | Represents a query parameter. Left parameters are indexed
-- by position, while right parameters are named.
type Param = Either Int String

-- | Shows the optimized SQL for the query.
instance Show (Query (Rel r)) where
  showsPrec _ query = shows (showSql query)

-- | Shows the optimized 'PrimQuery'.
showQuery :: Query (Rel r) -> String
showQuery = show . optimize . runQuery

-- | Shows the unoptimized 'PrimQuery'.
showQueryUnOpt :: Query (Rel r) -> String
showQueryUnOpt = show . runQuery

-- | Shows the optimized SQL query.
showSql :: Query (Rel r) -> String
showSql = show . ppSql . sqlQuery defaultSqlGenerator . optimize . runQuery 

-- | Shows the unoptimized SQL query.
showSqlUnOpt :: Query (Rel r) -> String
showSqlUnOpt = show . ppSql . sqlQuery defaultSqlGenerator . runQuery

-- | Get paramaters from a query in order.
queryParams :: Query (Rel r) -> [Param]
queryParams q = snd . indexParams . selectParams . toSelect $ q
  where
    -- Use foldr so we don't have to reverse parameter list built.
    indexParams = foldr' renumber (1, [])
    renumber (Just n) (idx, ps) = (idx, Right n : ps)
    renumber Nothing (idx, ps) = (idx + 1, Left idx : ps)
    toSelect = sqlQuery defaultSqlGenerator . optimize . runQuery 
    -- | All parameters that are in the select, in the textual order
    -- they will appear.
    selectParams :: SqlSelect -> [Maybe SqlName]
    selectParams select@(SqlSelect  { S.attrs = a, S.tables = t, S.criteria = c, S.groupby = g, S.orderby = o})
        = (attrParams a ++) . (tableParams t ++) . (criteriaParams c ++) .
            (groupByParams g ++) . orderByParams $ o
      where
        attrParams = getParams (exprParams . snd) 
        tableParams = getParams (selectParams . snd) 
        criteriaParams = getParams exprParams
        groupByParams (Just (Columns cs)) = getParams (exprParams . snd) cs
        groupByParams _ = []
        orderByParams = getParams (exprParams . fst) 
        getParams :: (a -> [Maybe SqlName]) -> [a] -> [Maybe SqlName]
        getParams f = concatMap f
        -- | All parameters in the expression, in the textual order
        -- in which they will appear.
        exprParams :: SqlExpr -> [Maybe SqlName]
        exprParams (ColumnSqlExpr _) = [] 
        exprParams (ConstSqlExpr _) = []
        exprParams (ParamSqlExpr p _) = [p] 
        exprParams (BinSqlExpr _ l r) = exprParams l ++ exprParams r
        exprParams (PrefixSqlExpr _ e) = exprParams e
        exprParams (PostfixSqlExpr _ e) = exprParams e
        exprParams (FunSqlExpr _ es) = (concatMap exprParams es)
        exprParams (CaseSqlExpr es e) =
          let caseExprs = concatMap (\(l, r) -> exprParams l ++ exprParams r) es
          in caseExprs ++ exprParams e
        exprParams (ListSqlExpr es) = concatMap exprParams es
        exprParams (ExistsSqlExpr select) = selectParams select
        exprParams PlaceHolderSqlExpr = []
        exprParams (ParensSqlExpr e) = exprParams e
        exprParams (CastSqlExpr _ e) = exprParams e
    selectParams (SqlBin _ l r) = selectParams l ++ selectParams r
    selectParams _ = []
