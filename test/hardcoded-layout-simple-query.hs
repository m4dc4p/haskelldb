import Database.HaskellDB.HSQL.ODBC
import Database.HaskellDB
import Database.HaskellDB.Query 

import Database.HaskellDB.HDBRec
import Database.HaskellDB.HDBRecUtils

import Random


-- create table test_tb1 (c11 int not null, c12 int null);

---------------------------------------------------------------------------
-- Tables
---------------------------------------------------------------------------
-------------------------------------
-- Table test_tb1
-------------------------------------
test_tb1 :: Table
    (HDBRecCons C11 (Expr Int)
     (HDBRecCons C12 (Expr (Maybe Int)) HDBRecTail))
test_tb1 = baseTable "test_tb1" $
           hdbMakeEntry C11 #
           hdbMakeEntry C12 

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

opts :: ODBCOptions
opts = ODBCOptions{dsn="", uid="", pwd=""}

-- run a test function
runTest f = odbcConnect opts f

--
-- A simple query
--

q = do
    tb1 <- table test_tb1
    project (c11 << tb1!c11 # c12 << tb1!c12)

newRec x y = c11 << constant x # c12 << constant y


printResults :: (Row row Int, Row row (Maybe Int)) => 
		[row (HDBRecCons C11 (Expr Int) (HDBRecCons C12 (Expr (Maybe Int)) HDBRecTail))] -> IO ()
printResults = mapM_ (\row -> putStrLn (show (row!.c11) ++ " " ++ show (row!.c12)))

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
{-
	     putStrLn "Tables:"
	     ts <- tables db
	     putStrLn (unlines ts)
	     cols <- describe db "test_tb1"
	     putStrLn "Columns in test_tb1"
	     putStrLn (unlines (map show cols))
	     putStrLn "Contents of test_tb1"
	     res <- query db q 
	     printResults res
	     (x::Int) <- randomIO
	     (y::Int) <- randomIO
	     let my = if even y then Just y else Nothing
	     insertNew db test_tb1 (newRec x my)
--	     insert db test_tb1 (project (newRec x my))
	     putStrLn $ "Contents of test_tb1 after inserting " ++ show (x,my)
-}
	     putStrLn "Contents of test_tb1"
	     res <- query db q 
	     printResults res

main = runTest bigTest
       
