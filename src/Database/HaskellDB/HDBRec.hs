module HDBRec where

data HDBRecTail = HDBRecTail
data HDBRecSep a b = HDBRecSep a b
data HDBRec r = HDBRec r

class HDBRecEntry a b | a -> b where
    fieldName :: a b -> String
    fieldValue :: a b -> b

class ShowRecRow r where
    showRecRow :: r -> [(String,ShowS)]

instance ShowRecRow HDBRecTail where
    showRecRow _ = []

instance (HDBRecEntry a b, Show b, ShowRecRow c) => ShowRecRow (HDBRecSep (a b) c) where
    showRecRow (HDBRecSep f fs) = 
	(fieldName f, const (show (fieldValue f))) : showRecRow fs

instance ShowRecRow r => ShowRecRow (HDBRec r) where
    showRecRow (HDBRec r) = showRecRow r
