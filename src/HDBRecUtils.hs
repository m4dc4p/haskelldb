-- | Utility functions for HDBRec.
module HDBRecUtils (hdbMakeEntry, 
		    hdbProject, 
		    hdbBaseTable,
		    hdbMakeRec,
		    (.<<.),
		    ( # )) where

import HDBRec
import Query
import PrimQuery

-- * Functions

-- Internal function used primarily in hdbProject and hdbBaseTable.
hdbMakeRec :: (HDBRecTail -> r) -> HDBRec r
hdbMakeRec f = HDBRec (f HDBRecTail)

-- | Constructs an entry with type and name of field.
hdbMakeEntry :: (Expr a -> b) -> String -> c -> HDBRecSep b c
hdbMakeEntry f n = HDBRecSep (f (attribute n)) 

-- | Project a HDBRec on the database.
hdbProject :: (ShowRecRow r) => (HDBRecTail -> r) -> Query (Rel r)
hdbProject = Query.project . hdbMakeRec

-- | Constructs a virtual table.
hdbBaseTable :: ShowRecRow r => String -> (HDBRecTail -> r) -> Table r
hdbBaseTable name = Query.baseTable name . hdbMakeRec

-- * Operators

infix  6 .<<.
infixr 5 #

-- | Links together a type and a value into an entry.
( .<<. ) :: (a -> b) -> a -> c -> HDBRecSep b c
f1 .<<. f2 = HDBRecSep $ f1 f2

-- | Links two records together.
( # ) :: (b -> c) -> (a -> b) -> a -> c
f1 # f2 = f1 . f2
