module FieldType (FieldDef, FieldType(..)) where

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
    deriving (Eq)

instance Show FieldType where
    show StringT = "String"
    show IntT = "Int"
    show IntegerT = "Integer"
    show DoubleT = "Double"
    show BoolT = "Bool"
