-- | Utility functions for HDBRec.
module HDBRecUtils (hdbMakeEntry, 
		    hdbProject, 
		    hdbBaseTable,
		    hdbMakeRec,
		    mkAttr,
		    ( << ),
		    ( # )) where

import HDBRec
import Query
import PrimQuery

-- * Functions

-- Internal function used primarily in hdbProject and hdbBaseTable.
hdbMakeRec :: (HDBRecTail -> r) -> HDBRec r
hdbMakeRec f = HDBRec (f HDBRecTail)

-- | Constructs an entry with type and name of field.
hdbMakeEntry :: f -> String -> c -> HDBRecCons f (Expr a) c
hdbMakeEntry f n = HDBRecCons f (attribute n)

-- | Project a HDBRec on the database.
hdbProject :: (ShowRecRow r) => (HDBRecTail -> r) -> Query (Rel r)
hdbProject = Query.project . hdbMakeRec

-- | Constructs a virtual table.
hdbBaseTable :: ShowRecRow r => String -> (HDBRecTail -> r) -> Table r
hdbBaseTable name = Query.baseTable name . hdbMakeRec

-- * Operators

infix  6 <<
infixr 5 #

-- | Links together a type and a value into an entry.
( << ) :: HDBRecEntry f (Expr a) => (forall r. HasField f r => Attr f r a) 
     -> Expr a -> (b -> HDBRecCons f (Expr a) b)
_ << x = HDBRecCons fieldTag x

-- | Links two fields together.
( # ) :: (b -> c) -> (a -> b) -> a -> c
f1 # f2 = f1 . f2

mkAttr :: (HDBRecEntry f (Expr a), HasField f r) => f -> Attr f r a
mkAttr = Attr . fieldName



