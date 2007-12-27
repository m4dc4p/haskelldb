-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.Sql.MySQL
-- Copyright   :  Bjorn Bringert 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- SQL generation for MySQL.
-- 
-----------------------------------------------------------
module Database.HaskellDB.Sql.MySQL (generator) where

import Database.HaskellDB.Sql
import Database.HaskellDB.Sql.Default
import Database.HaskellDB.Sql.Generate
import Database.HaskellDB.PrimQuery

generator :: SqlGenerator
generator = (mkSqlGenerator generator) {
              sqlBinary = mySqlBinary
            }

mySqlBinary :: RelOp -> SqlSelect -> SqlSelect -> SqlSelect
mySqlBinary Difference = mySqlDifference
mySqlBinary op = defaultSqlBinary generator op

{- Hack around the lack of "EXCEPT" in MySql -}
mySqlDifference :: SqlSelect -> SqlSelect -> SqlSelect
mySqlDifference sel1 sel2
 = (toSqlSelect sel1) { criteria = [PrefixSqlExpr "NOT" $ ExistsSqlExpr existsSql] }
     where existsSql = (toSqlSelect ((toSqlSelect sel2) { attrs = zipWith mkAttr names renames }))
                            { criteria = zipWith mkCond names renames }
           names = map fst $ attrs sel2 -- attrs sel1 should be the same, but it turned out to
                                        -- be undefined in the case I tried
           renames = map (++"_local") names
           mkAttr name rename = (rename, ColumnSqlExpr name)
           mkCond name rename = BinSqlExpr "=" (ColumnSqlExpr name) (ColumnSqlExpr rename)