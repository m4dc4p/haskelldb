import Database.HaskellDB
import Dp037.Test_tb1

import TestConnect

printTable db =
    do
    t <- query db (table test_tb1)
    mapM_ (putStrLn . showRow) t

showRow r = lpad 10 (show (r!c11)) ++ lpad 10 (showNullable (r!c12))

showNullable :: Show a => Maybe a -> String
showNullable = maybe "NULL" show

lpad :: Int -> String -> String
lpad x s = replicate (x - length s) ' ' ++ s


testInsert db = 
    do
    insert db test_tb1 (c11 << constant 157 # c12 << constant (Just 56))
    insert db test_tb1 (c11 << constant (-567) # c12 << constant Nothing)

testUpdate db = 
    do
    update db test_tb1 (\r -> r!c11 .==. constant 157) (setC12 (Just 18))
    where
    setC12 x r = c12 << constant x
testDelete db = 
    do
    delete db test_tb1 (\r -> r!c11 .==. constant 157 .||. isNull (r!c12))

test db = 
    do
    putStrLn "Before insert:"
    printTable db
    testInsert db
    putStrLn "After insert:"
    printTable db
    testUpdate db
    putStrLn "After update:"
    printTable db
    testDelete db
    putStrLn "After delete:"
    printTable db

main = argConnect test
