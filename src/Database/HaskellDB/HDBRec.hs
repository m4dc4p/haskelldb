-----------------------------------------------------------
-- |
-- Module      :  HDBRec
-- Copyright   :  HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
-- 
-- This is replacement for some TREX.
-----------------------------------------------------------
module Database.HaskellDB.HDBRec where

import Data.List

-- * Data declarations.

-- | Last entry in each record.
data HDBRecTail = HDBRecTail

-- | Constructor that adds a field to a record
-- f is the field tag, a is the field value and b is the rest of the record.
data HDBRecCons f a b = HDBRecCons f a b

-- | The type used for records throughout HaskellDB. This is a function
--   that takes a 'HDBRecTail' so that the user does not have to 
--   put a 'HDBRecTail' at the end of every record.
type HDBRec r = HDBRecTail -> r

-- * Class definitions.

-- | Each entry in a record needs to be an instance of this class.
-- Fundeps and two argument classes are used, this violates 
-- the haskell 98 standard. 
class HDBRecEntry f a | f -> a where
    fieldName :: f -> String
    fieldTag :: f

-- | The record @r@ has the field @f@ if there is an instance of
--   @HasField f r@.
class HasField f r
instance HasField f (HDBRecCons f a r)
instance HasField f r => HasField f (HDBRecCons g a r)

-- | A record must belong to this class to be showable.
class ShowRecRow r where
    showRecRow :: r -> [(String,ShowS)]

-- Last entry in each record will terminate the ShowrecRow recursion.
instance ShowRecRow HDBRecTail where
    showRecRow _ = []

-- Recurse a record and produce a showable tuple.
instance (HDBRecEntry a b, Show b, ShowRecRow c) => ShowRecRow (HDBRecCons a b c) where
    showRecRow (HDBRecCons f x fs) = (fieldName f, shows x) : showRecRow fs

instance ShowRecRow r => ShowRecRow (HDBRec r) where
    showRecRow r = showRecRow (r HDBRecTail)

{-
-- Quite likely to be totally useless. We'd actually like 
-- something like instance Row r => Show / Read r /Bjorn

instance ShowRecRow r => Show (HDBRec r) where
    showsPrec _ r = 
	showChar '[' . punct (showChar ',') (fields r) . showChar ']'
	    where fields = map (\ (x,y) -> showString x . 
				showChar '=' . y) . showRecRow
		  punct p ss r = foldr ($) r (intersperse p ss)

class ReadRecRow r where
    readRecRow :: [(String,String)] -> [(r,[(String,String)])]

instance ReadRecRow HDBRecTail where
    readRecRow xs = [(HDBRecTail,xs)]

instance (HDBRecEntry a b, Read b, ReadRecRow c) => 
    ReadRecRow (HDBRecCons a b c) where
    readRecRow [] = []
    readRecRow ((f,v):xs) = 
       let t = fieldTag
	   n = fieldName t
        in if n == f then
	       [(HDBRecCons t x r, xs') | (x,v') <- reads v, 
		                          (r,xs') <- readRecRow xs, 
		                          null v']
            else []

-}