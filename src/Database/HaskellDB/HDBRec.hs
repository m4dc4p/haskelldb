-- | This is a simple replacement for TREX.
module HDBRec where

-- * Data declarations.

-- | Last entry in each record.
data HDBRecTail = HDBRecTail

-- | Constructor that adds a field to a record
-- f is the field tag, a is the field value and b is the rest of the record.
data HDBRecCons f a b = HDBRecCons f a b

-- | Header of every record.
data HDBRec r = HDBRec r

-- * Class definitions.

-- | Each entry in a record needs to be an instance of this class.
-- Fundeps and two argument classes are used, this violates 
-- the haskell 98 standard. 
class HDBRecEntry f a | f -> a where
    fieldName :: f -> String
    fieldTag :: f

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
    showRecRow (HDBRecCons f x fs) = 
	(fieldName f, const (show x)) : showRecRow fs

instance ShowRecRow r => ShowRecRow (HDBRec r) where
    showRecRow (HDBRec r) = showRecRow r
