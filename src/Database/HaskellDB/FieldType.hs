module Database.HaskellDB.FieldType (FieldDef, FieldType(..)) where

import System.Time

{-
 TODO:

 - add date / time type(s)

-}

newtype Date = D (Int,Int,Int)
    deriving (Eq,Ord)
newtype Time = T (Int,Int,Int)
    deriving (Eq,Ord)
-- interestingly enough deriving Ord automatically for DateTime seems to work
-- I have tested it and it seems so anyway. / Chucky
newtype DateTime = DT (Date,Time)
    deriving (Eq,Ord)
type Timestamp = DateTime

-- we need to provide better definitions of show using showsPrec, and also
-- of readsPrec (to give us read)
instance Show Date where
    show (D (a,b,c)) = show a ++ "-" ++ show b ++ "-" ++ show c
{-instance Read Date where
    read s = (year,month,day)
	where
	year = read (take 4 xs)
	month = read (take 2 (drop 5 xs))
	day = read (take 2 (drop 8 xs))
-}
instance Show Time where
    show (T (a,b,c)) = show a ++ ":" ++ show b ++ ":" ++ show c

instance Show DateTime where
    show (DT (d,t)) = show d ++ " " ++ show t

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
