module Select

import Text.PrettyPrint

import qualified DB1.Int_tbl as Int_tbl
import qualified DB1.String_tbl as String_tbl

import DB1.String_tbl (String_tbl, string_tbl)
import qualified DB1.String_tbl as String_tbl

-- Select query examples


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
