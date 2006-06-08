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
-- $Revision: 1.25 $
-----------------------------------------------------------
module Database.HaskellDB.FieldType 
    (FieldDesc, FieldType(..), PrimShow(..), SQLShow(..)) where

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
    deriving (Eq,Show)

-- | defines a primitive show, that gives us the Haskell correct types from a
-- FieldType
class PrimShow a where
    pshow :: a -> String

instance PrimShow FieldType where
    pshow StringT = "String"
    pshow IntT = "Int"
    pshow IntegerT = "Integer"
    pshow DoubleT = "Double"
--    pshow BoolT = "Bool"
    pshow CalendarTimeT = "CalendarTime"
    pshow (BStrT a) = "BStr" ++ show a

-- | defines a show that is "SQL compatible" so to speak. Used to convert
-- FieldTypes to their SQL counterpart
class SQLShow a where
    sshow :: a -> String

instance SQLShow FieldType where
    sshow StringT = "text"
    sshow IntT = "int"
    sshow IntegerT = "bigint"
    sshow DoubleT = "double precision"
--    sshow BoolT = "bit"
    sshow CalendarTimeT = "timestamp"
    sshow (BStrT a) = "varchar(" ++ show a ++ ")"

instance Typeable CalendarTime where -- not available in standard libraries
    typeOf _ = mkTyConApp (mkTyCon "System.Time.CalendarTime") []

instance Typeable (BoundedString n) where
    typeOf _ = mkTyConApp (mkTyCon "Database.HaskellDB.BoundedString") []
