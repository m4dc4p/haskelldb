-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.Sql.PostgreSQL
-- Copyright   :  Bjorn Bringert 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- SQL generation for PostgreSQL.
-- 
-----------------------------------------------------------
module Database.HaskellDB.Sql.PostgreSQL (generator) where

import Database.HaskellDB.Sql
import Database.HaskellDB.Sql.Default
import Database.HaskellDB.Sql.Generate
import Database.HaskellDB.FieldType
import Database.HaskellDB.PrimQuery


generator :: SqlGenerator
generator = (mkSqlGenerator generator)
            {
             sqlSpecial = postgresqlSpecial,
             sqlType = postgresqlType
            }

postgresqlSpecial :: SpecialOp -> SqlSelect -> SqlSelect
postgresqlSpecial op q = defaultSqlSpecial generator op q      

postgresqlType :: FieldType -> SqlType
postgresqlType BoolT = SqlType "boolean"
postgresqlType t = defaultSqlType generator t