-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.Sql.Generate
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- The type of SQL generators.
-- 
-----------------------------------------------------------
module Database.HaskellDB.Sql.Generate (SqlGenerator(..)) where

import Database.HaskellDB.PrimQuery
import Database.HaskellDB.FieldType
import Database.HaskellDB.Sql


data SqlGenerator = SqlGenerator 
    {
     sqlQuery       :: PrimQuery -> SqlSelect,
     sqlUpdate      :: TableName -> [PrimExpr] -> Assoc -> SqlUpdate,
     sqlDelete      :: TableName -> [PrimExpr] -> SqlDelete,
     sqlInsert      :: TableName -> Assoc -> SqlInsert,
     sqlInsertQuery :: TableName -> PrimQuery -> SqlInsert,
     sqlCreateDB    :: String -> SqlCreate,
     sqlCreateTable :: TableName -> [(Attribute,FieldDesc)] -> SqlCreate,
     sqlDropDB      :: String -> SqlDrop,
     sqlDropTable   :: TableName -> SqlDrop,

     sqlEmpty       :: SqlSelect,
     sqlTable       :: TableName -> Scheme -> SqlSelect,
     sqlProject     :: Assoc -> SqlSelect -> SqlSelect,
     -- | Ensures non-aggregate expressions in the select are included in
     -- group by clause.
     sqlGroup       :: Assoc -> SqlSelect -> SqlSelect,
     sqlRestrict    :: PrimExpr -> SqlSelect -> SqlSelect,
     sqlBinary      :: RelOp -> SqlSelect -> SqlSelect -> SqlSelect,
     sqlSpecial     :: SpecialOp -> SqlSelect -> SqlSelect,

     sqlExpr        :: PrimExpr -> SqlExpr,
     sqlLiteral     :: Literal -> String,
     sqlType        :: FieldType -> SqlType,
     -- | Turn a string into a quoted string. Quote characters
     -- and any escaping are handled by this function.
     sqlQuote       :: String -> String
    }
