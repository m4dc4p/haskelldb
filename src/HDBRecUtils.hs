-----------------------------------------------------------
-- |
-- Module      :  HDBRecUtils
-- Copyright   :  HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This module contains utility functions for HDBRec.
-----------------------------------------------------------
module Database.HaskellDB.HDBRecUtils (hdbMakeEntry, 
		    mkAttr,
		    ( << ),
		    ( # )) where

import Database.HaskellDB.HDBRec
import Database.HaskellDB.Query

-- * Functions

-- | Constructs a table entry from a field tag
hdbMakeEntry :: HDBRecEntry f (Expr a) => 
		f -- ^ Field tag
	     -> b -- ^ Rest of the record
	     -> HDBRecCons f (Expr a) b
hdbMakeEntry f = HDBRecCons fieldTag (attribute (fieldName f))

-- | Make an 'Attr' for a field.
mkAttr :: (HDBRecEntry f (Expr a)) => 
	  f -- ^ Field tag
       -> Attr f r a
mkAttr = Attr . fieldName

-- * Operators

infix  6 <<
infixr 5 #

-- | Links together a type and a value into an entry.
( << ) :: HDBRecEntry f (Expr a) => 
	  Attr f r a 
       -> Expr a
       -> (b -> HDBRecCons f (Expr a) b)
_ << x = HDBRecCons fieldTag x

-- | Links two fields together.
( # ) :: (b -> c) -> (a -> b) -> a -> c
f1 # f2 = f1 . f2





