-----------------------------------------------------------
-- |
-- Module      :  DBLayout
-- Copyright   :  HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- Exports every function needed by DBDirect generated 
-- files
-----------------------------------------------------------

module Database.HaskellDB.DBLayout
    (module Database.HaskellDB.FieldType,
     module Database.HaskellDB.BoundedString,
     module Database.HaskellDB.DBSpec,
     CalendarTime,
     Expr, Table, Attr, baseTable,
     HDBRecCons,HDBRecTail,FieldTag,fieldName,
     hdbMakeEntry,mkAttr,( # ))
    where

import Database.HaskellDB.HDBRec(HDBRecCons,HDBRecTail,FieldTag,fieldName)
import Database.HaskellDB.HDBRecUtils(hdbMakeEntry,mkAttr,( # ))
import Database.HaskellDB.BoundedString
import System.Time (CalendarTime)
import Database.HaskellDB.Query (Expr, Table, Attr, baseTable)
import Database.HaskellDB.DBSpec
import Database.HaskellDB.FieldType
