-- | This is a simple replacement for TREX.
module HDBRec where

-- * Data declarations.

-- | Last entry in each record.
data HDBRecTail = HDBRecTail

-- | Constuctor placed between every entry in a record.
-- a is an entry and b is the remaining record.
data HDBRecSep a b = HDBRecSep a b

-- | Header of every record.
data HDBRec r = HDBRec r

-- * Class definitions.

-- | Each entry in a record needs to be an instance of this class.
-- Fundeps and two argument classes are used, this violates haskell 98 standard. 
class HDBRecEntry a b | a -> b where
    fieldName :: a b -> String
    fieldValue :: a b -> b

-- | A record must belong to this class to be showable.
class ShowRecRow r where
    showRecRow :: r -> [(String,ShowS)]

-- Last entry in each record will terminate the ShowrecRow recursion.
instance ShowRecRow HDBRecTail where
    showRecRow _ = []

-- Recurse a record and produce a showable tuple.
instance (HDBRecEntry a b, Show b, ShowRecRow c) => ShowRecRow (HDBRecSep (a b) c) where
    showRecRow (HDBRecSep f fs) = 
	(fieldName f, const (show (fieldValue f))) : showRecRow fs


instance ShowRecRow r => ShowRecRow (HDBRec r) where
    showRecRow (HDBRec r) = showRecRow r
