import HSQL_driver
import HaskellDB

import Database.ODBC.HSQL (SqlError, seErrorMsg, handleSql)
import HDBRec
import HDBRecUtils
import Query

---------------------------------------------------------------------------
-- Tables
---------------------------------------------------------------------------
-------------------------------------
-- Table test_tb1
-------------------------------------
test_tb1 :: Table
    (HDBRecSep (C11 (Expr Int))
     (HDBRecSep (C12 (Expr (Maybe Int))) HDBRecTail))

test_tb1 = hdbBaseTable "test_tb1" $
           hdbMakeEntry C11 "c11" .
           hdbMakeEntry C12 "c12"

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
-------------------------------------
-- C11 Field
-------------------------------------

data C11 a = C11 a deriving Show

instance HDBRecEntry C11 (Expr Int) where
    fieldName _ = "c11"
    fieldValue (C11 x) = x

class HasC11 r
instance HasC11 (HDBRecSep (C11 a) b)
instance HasC11 b => HasC11 (HDBRecSep a b)

c11 :: HasC11 r => Attr r Int
c11 = Attr "c11"

c11_ :: ShowRecRow r => Rel r -> Attr r Int -> b -> HDBRecSep (C11 (Expr Int)) b
c11_ t f = HDBRecSep $ C11 (t!f)

-------------------------------------
-- C12 Field
-------------------------------------

data C12 a = C12 a deriving Show

instance HDBRecEntry C12 (Expr (Maybe Int)) where
    fieldName _ = "c12"
    fieldValue (C12 x) = x

class HasC12 r
instance HasC12 (HDBRecSep (C12 a) b)
instance HasC12 b => HasC12 (HDBRecSep a b)

c12 :: HasC12 r => Attr r (Maybe Int)
c12 = Attr "c12"

c12_ :: ShowRecRow r => Rel r -> Attr r (Maybe Int) -> b -> HDBRecSep (C12 (Expr (Maybe Int))) b
c12_ t f = HDBRecSep $ C12 (t!f)

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
    hdbProject (c11_ tb1 c11 # c12_ tb1 c12)

ins = hdbMakeRec $ HDBRecSep (C11 (constant 42)) 
                   # HDBRecSep (C12 (constant 7))

-- weird type signature required by Hugs
handleQuery :: (HasC11 r, HasC12 r, Row row Int, Row row (Maybe Int)) => [row r] -> IO ()
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
       
