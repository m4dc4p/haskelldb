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

import Database.HaskellDB.Sql.Default
import Database.HaskellDB.Sql.Generate
import Database.HaskellDB.PrimQuery

generator :: SqlGenerator
generator = mkSqlGenerator generator 
