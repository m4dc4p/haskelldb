-----------------------------------------------------------
-- |
-- Module      :  DBLayout
-- Copyright   :  HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- Exports every function needed by DBDirect generated 
-- files
--
-- 
-----------------------------------------------------------

module Database.HaskellDB.DBLayout
    (module Database.HaskellDB.BoundedString,
     module Database.HaskellDB.DBSpec,
     module Data.HList.TypeEqGeneric1,
     CalendarTime,
     Expr, Table, baseTable, emptyTable,
     Record,HCons,HNil,LVPair,ShowLabel,showLabel,(.*.),emptyRecord,Proxy,proxy)
    where

import Data.HList
import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.TypeCastGeneric1
import Database.HaskellDB.BoundedString
import System.Time (CalendarTime)
import Database.HaskellDB.Query (Expr, Table, baseTable
                                , attribute, emptyTable)
import Database.HaskellDB.DBSpec
import Database.HaskellDB.FieldType (FieldType(..))
