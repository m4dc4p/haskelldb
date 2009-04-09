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
import System.Locale
import System.Time


generator :: SqlGenerator
generator = (mkSqlGenerator generator) { sqlSpecial = postgresqlSpecial
                                       , sqlType = postgresqlType
                                       , sqlLiteral = postgresqlLiteral
                                       }

postgresqlSpecial :: SpecialOp -> SqlSelect -> SqlSelect
postgresqlSpecial op q = defaultSqlSpecial generator op q      

-- Postgres > 7.1 wants a timezone with calendar time.
postgresqlLiteral :: Literal -> String
postgresqlLiteral (DateLit d) = defaultSqlQuote generator (formatCalendarTime defaultTimeLocale fmt d)
    where fmt = iso8601DateFormat (Just "%H:%M:%S %Z")
postgresqlLiteral l = defaultSqlLiteral generator l

postgresqlType :: FieldType -> SqlType
postgresqlType BoolT = SqlType "boolean"
-- Postgres < 7.1 assumed timestamp meant with a time zone, afterwards,
-- 'timestamp with time zone' is now required.
postgresqlType CalendarTimeT = SqlType "timestamp with time zone"
postgresqlType t = defaultSqlType generator t