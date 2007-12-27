-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.Sql.SQLite
-- Copyright   :  Bjorn Bringert 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- SQL generation for SQLite.
-- See <http://www.sqlite.org/lang.html> for documentation.
-- 
-----------------------------------------------------------
module Database.HaskellDB.Sql.SQLite (generator) where

import Database.HaskellDB.Sql.Default
import Database.HaskellDB.Sql.Generate
import Database.HaskellDB.PrimQuery

generator :: SqlGenerator
generator = (mkSqlGenerator generator)
            {
             sqlLiteral = literal
            }

literal :: Literal -> String
literal (StringLit s)   = quote s
literal DefaultLit      = "NULL"
literal (BoolLit True)  = "1"
literal (BoolLit False) = "0"
literal l = defaultSqlLiteral generator l

{-
From http://www.sqlite.org/lang_expr.html

"A string constant is formed by enclosing the string in single quotes ('). 
A single quote within the string can be encoded by putting two single quotes 
in a row - as in Pascal. C-style escapes using the backslash character 
are not supported because they are not standard SQL."
-}
quote :: String -> String
quote s = "'" ++ concatMap escape s ++ "'"

escape :: Char -> String
escape '\'' = "''"
escape c = [c]
