module FieldType where

{-
 TODO:

 - add date / time type(s)

-}

type FieldDef = (FieldType, Bool)

data FieldType = 
    StringT
    | IntT 
    | IntegerT
    | DoubleT
    | BoolT
    deriving (Eq, Show)