-----------------------------------------------------------
-- |
-- Module      :  HDBRec
-- Copyright   :  HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- This is a replacement for some of TREX.
--
-- $Revision: 1.25 $
-----------------------------------------------------------
module Database.HaskellDB.HDBRec 
    (
    -- * Record types
    RecNil(..), RecCons(..), Record
    -- * Record construction
    , (.=.), ( # )
    -- * Labels
    , FieldTag(..)
    -- * Record predicates and operations
    , HasField, SelectField(..)
    -- * Showing and reading records
    , ShowRecRow(..), ReadRecRow(..)
    ) where

import Data.List

infixr  5 #
infix   6 .=.

-- | The empty record.
data RecNil = RecNil deriving (Eq, Ord)

-- | Constructor that adds a field to a record.
-- f is the field tag, a is the field value and b is the rest of the record.
data RecCons f a b = RecCons a b deriving (Eq, Ord)

-- | The type used for records. This is a function
--   that takes a 'RecNil' so that the user does not have to 
--   put a 'RecNil' at the end of every record.
type Record r = RecNil -> r

-- * Record construction

-- | Creates an a record field from a label and a value
( .=. ) :: f             -- ^ Label
       -> a              -- ^ Value
       -> b              -- ^ Rest of the record
       -> RecCons f a b  -- ^ New record
_ .=. x = RecCons x

-- | Adds an entry to a record.
( # ) :: (b -> c) -> Record b -> Record c
( # ) = (.)

-- * Class definitions.

-- | Class for field labels.
class FieldTag f where
    -- | Gets the name of the label.
    fieldName :: f -> String


-- | The record @r@ has the field @f@ if there is an instance of
--   @HasField f r@.
class HasField f r
instance HasField f (RecCons f a r)
instance HasField f r => HasField f (RecCons g a r)
instance HasField f r => HasField f (Record r)

-- | Class for getting the value of a field from a record.
-- FIXME: would like the dependency f r -> a here, but
-- that makes Hugs complain about conflicting instaces
class SelectField f r a where
    -- | Gets the value of a field from a record.
    selectField :: f -- ^ Field label
		-> r -- ^ Record 
		-> a -- ^ Field value
    -- | Sets the value of a field in a record.
    setField :: f -- ^ Field label
	     -> a -- ^ New field value
	     -> r -- ^ Record
	     -> r -- ^ New record

instance SelectField f (RecCons f a r) a where
    selectField _ (RecCons x _) = x
    setField _ y (RecCons _ r) = RecCons y r

instance SelectField f r a => SelectField f (RecCons g b r) a where
    selectField f (RecCons _ r) = selectField f r
    setField l y (RecCons f r) = RecCons f (setField l y r)

instance SelectField f r a => SelectField f (Record r) a where
    selectField f r = selectField f (r RecNil)
    setField f y r = \e -> setField f y (r e)


-- * Showing rows 

-- | Get the label name of a record entry.
consFieldName :: FieldTag f => RecCons f a r -> String
consFieldName (_::RecCons f a r) = fieldName (undefined::f)

-- | Convert a record to a list of label names and field values.
class ShowRecRow r where
    showRecRow :: r -> [(String,ShowS)]

-- Last entry in each record will terminate the ShowrecRow recursion.
instance ShowRecRow RecNil where
    showRecRow _ = []

-- Recurse a record and produce a showable tuple.
instance (FieldTag a, 
	  Show b, 
	  ShowRecRow c) => ShowRecRow (RecCons a b c) where
    showRecRow r@(RecCons x fs) = (consFieldName r, shows x) : showRecRow fs

instance ShowRecRow r => ShowRecRow (Record r) where
    showRecRow r = showRecRow (r RecNil)



instance Show r => Show (Record r) where
    showsPrec x r = showsPrec x (r RecNil)

-- probably not terribly efficient
showsShowRecRow :: ShowRecRow r => r -> ShowS 
showsShowRecRow r = shows $ [(f,v "") | (f,v) <- showRecRow r]

instance Show RecNil where
    showsPrec _ r = showsShowRecRow r

instance  (FieldTag a, Show b, ShowRecRow c) => Show (RecCons a b c) where
    showsPrec _ r = showsShowRecRow r


-- * Reading rows

class ReadRecRow r where
    -- | Convert a list of labels and strins representating values
    --   to a record.
    readRecRow :: [(String,String)] -> [(r,[(String,String)])]

instance ReadRecRow RecNil where
    readRecRow xs = [(RecNil,xs)]

instance (FieldTag a, 
	  Read b, 
	  ReadRecRow c) => ReadRecRow (RecCons a b c) where
    readRecRow [] = []
    readRecRow xs = let res = readRecEntry xs (fst $ head res) in res

readRecEntry :: (Read a, FieldTag f, ReadRecRow r) => 
		[(String,String)] 
	     -> RecCons f a r   -- ^ Dummy to get return type right
	     -> [(RecCons f a r,[(String,String)])]
readRecEntry ((f,v):xs) r | f == consFieldName r = res
			  | otherwise = []
    where
    res = [(RecCons x r, xs') | (x,"") <- reads v, 
	   (r,xs') <- readRecRow xs]


readsReadRecRow :: ReadRecRow r => ReadS r
readsReadRecRow s = [(r,"") | (l,"") <- reads s, (r,[]) <- readRecRow l]

instance ReadRecRow r => Read (Record r) where
    readsPrec _ s = [(const r, rs) | (r,rs) <- readsReadRecRow s]

instance Read RecNil where
   readsPrec _ = readsReadRecRow

instance (FieldTag a, Read b, ReadRecRow c) => Read (RecCons a b c) where
    readsPrec _ s = readsReadRecRow s
