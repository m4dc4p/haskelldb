-----------------------------------------------------------
-- |
-- Module      :  FieldType
-- Copyright   :  HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Defines the types of database columns, and functions
-- for converting these between HSQL and internal formats
--
-- 
-----------------------------------------------------------
module Database.HaskellDB.FieldType 
    (FieldDesc, FieldType(..), toHaskellType) where

import Data.Dynamic
import System.Time

import Database.HaskellDB.BoundedString

-- | The type and @nullable@ flag of a database column
type FieldDesc = (FieldType, Bool)

-- | A database column type
data FieldType = 
    StringT
    | IntT 
    | IntegerT
    | DoubleT
    | BoolT
    | CalendarTimeT
    | BStrT Int
    deriving (Eq,Ord,Show,Read)

toHaskellType :: FieldType -> String
toHaskellType StringT = "String"
toHaskellType IntT = "Int"
toHaskellType IntegerT = "Integer"
toHaskellType DoubleT = "Double"
toHaskellType BoolT = "Bool"
toHaskellType CalendarTimeT = "CalendarTime"
toHaskellType (BStrT a) = "BStr" ++ show a

instance Typeable CalendarTime where -- not available in standard libraries
    typeOf _ = mkTyConApp (mkTyCon "System.Time.CalendarTime") []

instance Typeable (BoundedString n) where
    typeOf _ = mkTyConApp (mkTyCon "Database.HaskellDB.BoundedString") []
