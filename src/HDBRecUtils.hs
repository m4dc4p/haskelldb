module HDBRecUtils where

import HDBRec
import Query
import PrimQuery

--
-- Utility functions
--

hdbMakeRec :: (HDBRecTail -> r) -> HDBRec r
hdbMakeRec f = HDBRec (f HDBRecTail)

hdbMakeEntry :: (Expr a -> b) -> String -> c -> HDBRecSep b c
hdbMakeEntry f n = HDBRecSep (f (attribute n)) 

hdbProject :: (ShowRecRow r) => (HDBRecTail -> r) -> Query (Rel r)
hdbProject = Query.project . hdbMakeRec

hdbBaseTable :: ShowRecRow r => String -> (HDBRecTail -> r) -> Table r
hdbBaseTable name = Query.baseTable name . hdbMakeRec


infix  6 .<<.
infixr 5 #

( .<<. ) :: (a -> b) -> a -> c -> HDBRecSep b c
f1 .<<. f2 = HDBRecSep $ f1 f2

( # ) :: (b -> c) -> (a -> b) -> a -> c
f1 # f2 = f1 . f2