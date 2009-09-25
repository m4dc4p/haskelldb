{-| This module demonstrates SELECT query operators. -}

import Text.PrettyPrint

import Database.HaskellDB
import Database.HaskellDB.PrintQuery (ppSql)
import Data.HList

import DB1.Int_tbl (Int_tbl, int_tbl)
import qualified DB1.Int_tbl as Int_tbl

import DB1.String_tbl (String_tbl, string_tbl)
import qualified DB1.String_tbl as String_tbl

-- Select query examples


-- | Demonstrates use of aggregate count() function
-- in a projection.
demoCount = demo "demoCount" $ do
  t1 <- table int_tbl
  t2 <- table int_tbl
  order [asc t1 Int_tbl.f02]
  project $ Int_tbl.f02 .=. count(t1 .!. Int_tbl.f02)
              .*. Int_tbl.f01 .=. (t2 .!. Int_tbl.f01) 
              .*. emptyRecord

-- | Demonstrate using the 'unique' operator. This query
-- will be grouped by all the columns in the projection.
uniqueUsage = demo "uniqueUsage" $ do
  t1 <- table int_tbl
  t2 <- project $ Int_tbl.f01 .=. t1 .!. Int_tbl.f01 
        .*. emptyRecord
  unique
  return t2

-- | Shows the use of the copyAll operator.
-- This query projects all the columns in string_tbl.
copyAllUsage = demo "copyUsage" $ do 
  t2 <- table string_tbl
  project $ copyAll t2 

-- | Shows usage of the copy operator. This
-- query projects columns from int_tbl and string_tbl. Note
-- that copy will NOT rename columns, so you can get column
-- collisions if you are not careful.
copyUsage = demo "copyUsage" $ do 
  t2 <- table string_tbl
  t1 <- table int_tbl
  project $ copy Int_tbl.f01 t1 
              .*. copy String_tbl.f02 t2 
              .*. emptyRecord


-- 	      -- * Operators
-- 	     , (.==.) , (.<>.), (.<.), (.<=.), (.>.), (.>=.)
-- 	     , (.&&.) , (.||.)
-- 	     , mul, (./.), (.+.), (.-.), (.%.), (.++.)
-- 	      -- * Function declarations
-- 	     , project, restrict, table, unique
-- 	     , union, intersect, divide, minus
-- 	     , _not, like, _in, cat, _length
-- 	     , isNull, notNull
-- 	     , fromNull
-- 	     , constant, constJust
-- 	     , param, namedParam, Args, func, constNull, cast
-- 	     , toStr, coerce
-- 	     , count, _sum, _max, _min, avg
-- 	     , literal
-- 	     , stddev, stddevP, variance, varianceP
-- 	     , asc, desc, order
-- 	     , top
-- 	     , _case
-- 	     , _default

main = putStrLn . render . stack $ [demoCount
                                   , copyUsage
                                   , uniqueUsage
                                   , copyAllUsage
                                   , copyUsage]

demo name qry = text name <> colon <+> ppSql qry
stack [] = empty
stack docs = foldl1 (\doc e -> e $+$ space $+$ doc) docs


