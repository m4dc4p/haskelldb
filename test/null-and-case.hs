import Database.HaskellDB

import TestConnect

import Dp037.Hdb_test_t1

q1 = do
     t <- table hdb_test_t1
     restrict (fromNull (constant "") (t!t1f01) `like` constant "foo")
     return t

test db = 
    do
    putStrLn $ show $ showSql q1
    rs <- query db q1
    mapM_ (putStrLn . showRow) rs

showRow r = show (r!t1f01)

main = argConnect test
