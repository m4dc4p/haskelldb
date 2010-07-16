{-# LANGUAGE FlexibleContexts, UndecidableInstances, TypeSynonymInstances, FlexibleInstances
  , MultiParamTypeClasses #-}
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
    (FieldDesc, FieldType(..), toHaskellType, ExprType(..)
    , ExprTypes(..), queryFields) where

import Data.Dynamic
import System.Time

import Database.HaskellDB.HDBRec (RecCons(..), Record, RecNil(..), ShowLabels)
import Database.HaskellDB.BoundedString
import Database.HaskellDB.BoundedList (listBound, Size)
import Database.HaskellDB.Query (Expr, Rel, runQueryRel, Query, labels)

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

-- | Class which retrieves a field description from a given type.
-- Instances are provided for most concrete types. Instances
-- for Maybe automatically make the field nullable, and instances
-- for all (Expr a) types where a has an ExprType instance allows
-- type information to be recovered from a given column expression.
class ExprType e where
  fromHaskellType :: e -> FieldDesc

-- | Class which returns a list of field descriptions. Gets the
-- descriptions of all columns in a Record/query. Most useful when
-- the columns associated with each field in a (Rel r) type must be
-- recovered. Note that this occurs at the type level only and no
-- values are inspected.
class ExprTypes r where
  fromHaskellTypes :: r -> [FieldDesc]

toHaskellType :: FieldType -> String
toHaskellType StringT = "String"
toHaskellType IntT = "Int"
toHaskellType IntegerT = "Integer"
toHaskellType DoubleT = "Double"
toHaskellType BoolT = "Bool"
toHaskellType CalendarTimeT = "CalendarTime"
toHaskellType (BStrT a) = "BStr" ++ show a

-- | Given a query, returns a list of the field names and their
-- types used by the query. Useful for recovering field information
-- once a query has been built up. 
queryFields :: (ShowLabels r, ExprTypes r) => Query (Rel r) -> [(String, FieldDesc)]
queryFields def = zip (labels query) types
  where
    query = unRel . snd . runQueryRel $ def
    types = fromHaskellTypes query 
    unRel :: (Rel r) -> r
    unRel r = undefined -- Only used to get to type-level information.

instance Typeable CalendarTime where -- not available in standard libraries
    typeOf _ = mkTyConApp (mkTyCon "System.Time.CalendarTime") []

instance Typeable (BoundedString n) where
    typeOf _ = mkTyConApp (mkTyCon "Database.HaskellDB.BoundedString") []

instance (ExprType a) => ExprType (Maybe a) where
  fromHaskellType ~(Just e) = ((fst . fromHaskellType $ e), True)

instance (ExprType a) => ExprType (Expr a) where
  fromHaskellType e =
    let unExpr :: Expr a -> a
        unExpr _ = undefined
    in fromHaskellType . unExpr $ e

instance (ExprType a) => ExprType (Rel a) where
  fromHaskellType e =
    let unRel :: Rel a -> a
        unRel _ = undefined
    in fromHaskellType . unRel $ e
    
instance ExprType Bool where
  fromHaskellType _ = (BoolT, False)

instance ExprType String where
  fromHaskellType _ = (StringT, False)
  
instance ExprType Int where
  fromHaskellType _ = (IntT, False)

instance ExprType Integer where
  fromHaskellType _ = (IntegerT, False)

instance ExprType Double where
  fromHaskellType _ = (DoubleT, False)

instance ExprType CalendarTime where
  fromHaskellType _ = (CalendarTimeT, False)

instance (Size n) => ExprType (BoundedString n) where
  fromHaskellType b = (BStrT (listBound b), False)

instance ExprTypes RecNil where
  fromHaskellTypes _ = []

instance (ExprType e, ExprTypes r) => ExprTypes (RecCons f e r) where
  fromHaskellTypes ~f@(RecCons e r) =
    let getFieldType :: RecCons f a r -> a
        getFieldType = undefined
    in (fromHaskellType . getFieldType $ f) : fromHaskellTypes r

instance (ExprTypes r) => ExprTypes (Record r) where
  fromHaskellTypes r = fromHaskellTypes (r RecNil)

instance (ExprTypes r) => ExprTypes (Rel r) where
  fromHaskellTypes r =
    let unRel :: Rel a -> a
        unRel _ = undefined
    in fromHaskellTypes . unRel $ r
