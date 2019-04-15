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
import Control.Arrow


generator :: SqlGenerator
generator = (mkSqlGenerator generator) { sqlSpecial = postgresqlSpecial
                                       , sqlType = postgresqlType
                                       , sqlLiteral = postgresqlLiteral
                                       , sqlExpr = postgresqlExpr
                                       , sqlTable = postgresqlTable
                                       , sqlInsert = postgresqlInsert
                                       , sqlDelete = postgresqlDelete
                                       , sqlUpdate = postgresqlUpdate
                                       }

postgresqlUpdate :: TableName -> [PrimExpr] -> Assoc -> SqlUpdate
postgresqlUpdate name exprs = defaultSqlUpdate generator name exprs . map (first quote)

postgresqlTable :: TableName -> Scheme -> SqlSelect
postgresqlTable tablename scheme = defaultSqlTable generator (quote tablename) (map quote scheme)

postgresqlDelete :: TableName -> [PrimExpr] -> SqlDelete
postgresqlDelete = defaultSqlDelete generator . quote

postgresqlInsert :: TableName -> Assoc -> SqlInsert
postgresqlInsert n = defaultSqlInsert generator (quote n) . map (first quote)

postgresqlSpecial :: SpecialOp -> SqlSelect -> SqlSelect
postgresqlSpecial op q = defaultSqlSpecial generator op q

-- Postgres > 7.1 wants a timezone with calendar time.
postgresqlLiteral :: Literal -> String
postgresqlLiteral (DateLit d) = defaultSqlQuote generator (formatCalendarTime defaultTimeLocale fmt d)
    where fmt = iso8601DateFormat (Just "%H:%M:%S %Z")
postgresqlLiteral (StringLit l) = "E" ++ (defaultSqlLiteral generator (StringLit l))
postgresqlLiteral l = defaultSqlLiteral generator l

postgresqlType :: FieldType -> SqlType
postgresqlType BoolT = SqlType "boolean"
postgresqlType t = defaultSqlType generator t

postgresqlExpr :: PrimExpr -> SqlExpr
postgresqlExpr (BinExpr OpMod e1 e2) =
    let e1S = defaultSqlExpr generator e1
        e2S = defaultSqlExpr generator e2
    in BinSqlExpr "%" e1S e2S
postgresqlExpr (AttrExpr n) = defaultSqlExpr generator $ AttrExpr $ quote n
postgresqlExpr e = defaultSqlExpr generator e

quote :: String -> String
quote x@('"':_) = x
quote x = case break (=='.') x of
  (l,[]) -> q l
  (l,r)  -> q l ++ "." ++ q (drop 1 r)
  where q w = "\"" ++ w ++ "\""
