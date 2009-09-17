
import Text.PrettyPrint

import Database.HaskellDB
import Database.HaskellDB.PrintQuery (ppSql)

import DB1.Int_tbl (Int_tbl, int_tbl)
import qualified DB1.Int_tbl as Int_tbl

-- Select query examples

-- | Demonstrates use of aggregate count() function
-- in a projection.
demoCount = demo "demoCount" qry
  where qry = do
          t1 <- table int_tbl
          t2 <- table int_tbl
          order [asc t1 Int_tbl.f02]
          project $ Int_tbl.f02 .=. count(t1 .!. Int_tbl.f02) 
                      .*. Int_tbl.f01 .=. (t2 .!. Int_tbl.f01) 
                      .*. emptyRecord

-- | Demonstrate using the 'unique' operator.    
uniqueUsage = demo "uniqueUsage" $ 
  do
    p <- table int_tbl
    r <- project (Int_tbl.f01 .=. (p .!. Int_tbl.f01) .*. emptyRecord)
    unique
    return r

main = putStrLn . render . stack $ [demoCount
                                   , uniqueUsage]

demo name qry = text name <> colon <+> ppSql qry
stack [] = empty
stack docs = foldl1 (\doc e -> e $+$ space $+$ doc) docs


