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
--
-- $Revision: 1.4 $
-----------------------------------------------------------

module Database.HaskellDB.DBLayout
    (module Database.HaskellDB.FieldType,
     module Database.HaskellDB.BoundedString,
     module Database.HaskellDB.DBSpec,
     CalendarTime,
     Expr, Table, Attr, baseTable,
     RecCons,RecNil,FieldTag,fieldName,
     hdbMakeEntry,mkAttr,( # ))
    where

import Database.HaskellDB.HDBRec(RecCons(..),RecNil,FieldTag,fieldName, ( # ))
import Database.HaskellDB.BoundedString
import System.Time (CalendarTime)
import Database.HaskellDB.Query (Expr, Table, Attr(..), baseTable, attribute)
import Database.HaskellDB.DBSpec
import Database.HaskellDB.FieldType

-- | Constructs a table entry from a field tag
hdbMakeEntry :: FieldTag f => 
		f -- ^ Field tag
	     -> b -- ^ Rest of the record
	     -> RecCons f (Expr a) b
hdbMakeEntry f = RecCons (attribute (fieldName f))

-- | Make an 'Attr' for a field.
mkAttr :: FieldTag f => 
	  f -- ^ Field tag
       -> Attr f a
mkAttr = Attr . fieldName
