import HSQL_driver
import HaskellDB

import Database.ODBC.HSQL (SqlError, seErrorMsg, handleSql)
import HDBRec
import HDBRecUtils
import Database
import Query

---------------------------------------------------------------------------
-- Tables
---------------------------------------------------------------------------
-------------------------------------
-- Table test_tb1
-------------------------------------
test_tb1 :: Table
    (HDBRecCons C11 (Expr Int)
     (HDBRecCons C12 (Expr (Maybe Int)) HDBRecTail))

test_tb1 = hdbBaseTable "test_tb1" $
           hdbMakeEntry C11 "c11" .
           hdbMakeEntry C12 "c12"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
-------------------------------------
-- C11 Field
-------------------------------------

data C11 = C11

instance HDBRecEntry C11 (Expr Int) where
    fieldTag = C11
    fieldName _ = "c11"

c11 :: HasField C11 r => Attr C11 r Int
c11 = mkAttr C11

-------------------------------------
-- C12 Field
-------------------------------------

data C12 = C12

instance HDBRecEntry C12 (Expr (Maybe Int)) where
    fieldTag = C12
    fieldName _ = "c12"

c12 :: HasField C12 r => Attr C12 r (Maybe Int)
c12 = mkAttr C12


--
-- Test utilites
--

exError :: SqlError -> IO a
exError = error . seErrorMsg

opts :: ODBCOptions
opts = ODBCOptions{dsn="mysql-dp037", uid="dp037", pwd="teent333"}

-- run a test function
runTest f = handleSql exError $ odbcConnect opts f

--
-- A simple query
--

q = do
    tb1 <- table test_tb1
    hdbProject (c11 << tb1!c11 # c12 << tb1!c12)

ins = hdbMakeRec $ c11 << constant 42 # c12 << constant (Just 7)


handleQuery :: (HasField C11 r, HasField C12 r, Row row Int, Row row (Maybe Int)) => [row r] -> IO ()
handleQuery = mapM_ (\row -> putStrLn (show (row!.c11) ++ " " ++ show (row!.c12)))

--
-- Testing db layout functions
--

-- run 'tables'
listTables :: IO ()
listTables = runTest tables >>= putStr . unlines

-- run 'describe'
describeTable :: String -> IO ()
describeTable table = runTest (\db -> describe db table) 
		      >>= putStr . unlines . map show




bigTest db = do
	     putStrLn ("Connected to: " ++ dsn opts ++ "\n")
	     putStrLn "Tables:"
	     ts <- tables db
	     putStrLn (unlines ts)
	     cols <- describe db "test_tb1"
	     putStrLn "Columns in test_tb1"
	     putStrLn (unlines (map show cols))
	     res <- query db q 
	     handleQuery res
--       insertNew db test_tb1 ins

main = runTest bigTest
       
