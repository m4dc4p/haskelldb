{- |
   Load this module in GHCi and use the ":pSQL" command
   to evaluate the queries. 
-}
module Select

where

-- import HaskellDB operators and
-- functions.
import Database.HaskellDB

-- Import table type and definition directly.
import DB1.Int_tbl (Int_tbl, int_tbl)
import DB1.String_tbl (String_tbl, string_tbl)

-- Import Hdb_t1 directly so we can use its columns
-- as labels in projections easily.
import DB1.Hdb_t1 (t1f01, t1f02, t1f05, t1f06
                  , t1f09, t1f10, t1f13, t1f14, t1f21, t1f22)

-- Import columns on tables with a qualified name.
import qualified DB1.Int_tbl as Int_tbl
import qualified DB1.String_tbl as String_tbl

import DB1.String_tbl (String_tbl, string_tbl)
import qualified DB1.String_tbl as String_tbl

-- Select query examples

-- | Demonstrates use of aggregate count() function
-- in a projection.
demoCount = do
  t1 <- table int_tbl
  t2 <- table int_tbl
  order [asc t1 Int_tbl.f02]
  project $ Int_tbl.f02 << count(t1 ! Int_tbl.f02)
              # Int_tbl.f01 << (t2 ! Int_tbl.f01) 

-- | Demonstrate using the 'unique' operator. This query
-- will be grouped by all the columns in the projection.
uniqueUsage = do
  t1 <- table int_tbl
  t2 <- project $ Int_tbl.f01 << t1 ! Int_tbl.f01 
  unique
  return t2

-- | Shows the use of the copyAll operator.
-- This query projects all the columns in string_tbl.
copyAllUsage = do 
  t2 <- table string_tbl
  project $ copyAll t2 

-- | Shows usage of the copy operator. This
-- query projects columns from int_tbl and string_tbl. Note
-- that copy will NOT rename columns, so you can get column
-- collisions if you are not careful.
copyUsage = do 
  t2 <- table string_tbl
  t1 <- table int_tbl
  project $ copy Int_tbl.f01 t1 
              # copy String_tbl.f02 t2 

-- | Select all columns from Int_tbl
select1 = do
  intTbl <- table int_tbl
  project $ copyAll intTbl

-- | Select some columns from Int_tbl
-- and some from String_tbl. No restriction is
-- specified between the two tables, so 
-- this will be a cross join. 
select2 = do
  intTbl <- table int_tbl
  strTbl <- table string_tbl
  project $ t1f05 << intTbl ! Int_tbl.f01 
          # t1f02 << strTbl ! String_tbl.f02
          # t1f05 << intTbl ! Int_tbl.f03
          # t1f02 << strTbl ! String_tbl.f04
            
-- | Select values from Int_tbl and 
-- cast them to text. 
select3 = do
  intTbl <- table int_tbl
  project $ t1f02 << cast "text" (intTbl ! Int_tbl.f01) 

-- | Select values from Int_tbl and String_tbl, cast
-- the int values to text, and join them to related
-- values on String_tbl. Finally, project two columns
-- from the join.
select4 = do
  intTbl <- table int_tbl
  strTbl <- table string_tbl
  restrict $ cast "text" (intTbl ! Int_tbl.f01) .==. 
           strTbl ! String_tbl.f01
  project $ t1f05 << intTbl ! Int_tbl.f01
          # t1f06 << intTbl ! Int_tbl.f02
          # t1f01 << strTbl ! String_tbl.f01
          # t1f02 << strTbl ! String_tbl.f02
