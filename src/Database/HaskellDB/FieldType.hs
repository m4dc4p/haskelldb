module FieldType where

{-
 TODO:

 - add datae / time type(s)


-}

type FieldDef = (FieldType, Bool)

data FieldType = 
    StringT
    | IntT 
    | IntegerT
    | DoubleT
    | BoolT
