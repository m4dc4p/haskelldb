import Database.HaskellDB
import Dp037.Test_tb1

--import Database.HaskellDB.HSQL.PostgreSQL
import Database.HaskellDB.HSQL.ODBC

opts = ODBCOptions { dsn = "mysql-dp037", uid = "dp037", pwd = "teent333" }
withDB = odbcConnect opts


printTable db =
    do
    t <- query db (table test_tb1)
    mapM_ (putStrLn . showRow) t

showRow r = lpad 10 (show (r!.c11)) ++ lpad 10 (showNullable (r!.c12))

showNullable :: Show a => Maybe a -> String
showNullable = maybe "NULL" show

lpad :: Int -> String -> String
lpad x s = replicate (x - length s) ' ' ++ s


testInsert db = 
    do
    insert db test_tb1 (c11 << constant 157 # c12 << constant (Just 56))
    insert db test_tb1 (c11 << constant (-567) # c12 << constant Nothing)

testUpdate db = return ()
testDelete db = return ()


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

main = withDB test