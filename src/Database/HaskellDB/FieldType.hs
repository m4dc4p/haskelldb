module Database.HaskellDB.FieldType 
    (FieldDef, FieldType(..), mkCalendarTime) where

import Data.Dynamic
import System.Time

{-
 TODO:

 - add date / time type(s)
 DEVELOPMENT OF THIS HAS BEEN MOVED TO DateTypes.hs

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
    | CalendarTimeT
    deriving (Eq)

instance Show FieldType where
    show StringT = "String"
    show IntT = "Int"
    show IntegerT = "Integer"
    show DoubleT = "Double"
    show BoolT = "Bool"
    show CalendarTimeT = "CalendarTime"

-- | Creates a CalendarTime from a ClockTime
-- | This loses the time zone and assumes UTC. :(
-- | A probable fix could be to make DbDirect aware of which time zone the
-- | server is in and handle it here
-- | This is just a function synonym for now
mkCalendarTime :: ClockTime -> CalendarTime
mkCalendarTime = toUTCTime

instance Typeable CalendarTime where
    typeOf _ = mkAppTy (mkTyCon "Database.HaskellDB.FieldType Error") []