import Database.HaskellDB
import Database.HaskellDB.HSQL.MySQL
import Control.Exception
import Dp037.Test_tb1

--opts = ODBCOptions{dsn="", uid="", pwd=""}
--withDB f = odbcConnect opts f

opts = MySQLOptions{server="localhost", db="", uid="", pwd=""}
withDB f = mysqlConnect opts f

printRecord r = putStrLn $ show (r!.c11) ++ " " ++ show (r!.c12)

printRecords db = do
		  rs <- query db (table test_tb1)
		  mapM printRecord rs

testTrans db = do
	       insert db test_tb1 (c11 << constant 2 # c12 << nullable (-67))
	       fail "oops, that didn't work out"

test db = do
	  putStrLn "before transaction:"
	  printRecords db
	  catchJust userErrors (transaction db (testTrans db)) (\e -> putStrLn $ "exception: " ++ e)
	  putStrLn "after transaction:"
	  printRecords db


main = withDB test