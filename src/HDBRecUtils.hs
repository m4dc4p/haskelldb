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
		    ( # ),
		    selectField,
		    (<<-)
		   ) where

import Database.HaskellDB.HDBRec
import Database.HaskellDB.Query

-- * Functions

-- | Constructs a table entry from a field tag
hdbMakeEntry :: FieldTag f => 
		f -- ^ Field tag
	     -> b -- ^ Rest of the record
	     -> HDBRecCons f (Expr a) b
hdbMakeEntry f = HDBRecCons (attribute (fieldName f))

-- | Make an 'Attr' for a field.
mkAttr :: FieldTag f => 
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
_ << x = HDBRecCons x

-- | Links two fields together.
( # ) :: (b -> c) -> (a -> b) -> a -> c
f1 # f2 = f1 . f2


-- beginnings of a field selector function

class SelectField f r a where
    selectField :: Attr f r1 a -> r -> a

instance SelectField f (HDBRecCons f a r) a where
    selectField _ (HDBRecCons x _) = x

instance SelectField f r a => SelectField f (HDBRecCons g b r) a where
    selectField f (HDBRecCons _ r) = selectField f r


--  FIXME: remove
-- for testing selectField
( <<- ) :: HDBRecEntry f (Expr a) => 
        Attr f r a 
       -> a
       -> (b -> HDBRecCons f a b)
_ <<- x = HDBRecCons x