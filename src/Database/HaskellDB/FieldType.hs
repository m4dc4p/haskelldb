module FieldType where

{-
 TODO:

 - add date / time type(s)

-}

-- | The type and @nullable@ flag of a database column
type FieldDef = (FieldType, Bool)

-- | A database column type
data FieldType = 
    StringT
    | IntT 
    | IntegerT
    | DoubleT
    | BoolT
    deriving (Eq, Show)