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
import Database.HaskellDB.Query (Expr, Table, Attr(..)
                                , baseTable, attribute, (<<), emptyTable)
import Database.HaskellDB.DBSpec
import Database.HaskellDB.FieldType (FieldType(..))

-- | Constructs a table entry from a field tag
hdbMakeEntry :: FieldTag f =>
              f -- ^ Field tag
           -> Record (RecCons f (Expr a) RecNil)
hdbMakeEntry f = undefined << attribute (fieldName f)

-- | Make an 'Attr' for a field.
mkAttr :: FieldTag f =>
         f -- ^ Field tag
        -> Attr f a
mkAttr = Attr . fieldName