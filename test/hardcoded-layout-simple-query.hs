import Database.HaskellDB
import Database.HaskellDB.DBLayout

import TestConnect

import Random


-- create table test_tb1 (c11 int not null, c12 int null);

---------------------------------------------------------------------------
-- Tables
---------------------------------------------------------------------------
-------------------------------------
-- Table test_tb1
-------------------------------------
test_tb1 :: Table
    (RecCons C11 (Expr Int)
     (RecCons C12 (Expr (Maybe Int)) RecNil))
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
instance FieldTag C11 where fieldName _ = "c11"

c11 :: Attr C11 Int
c11 = mkAttr C11

-------------------------------------
-- C12 Field
-------------------------------------

data C12 = C12
instance FieldTag C12 where fieldName _ = "c12"

c12 :: Attr C12 (Maybe Int)
c12 = mkAttr C12


--
-- A simple query
--

q = do
    tb1 <- table test_tb1
    project (c11 << tb1!c11 # c12 << tb1!c12)

newRec x y = c11 << constant x # c12 << constant y

printResults rs = mapM_ (\row -> putStrLn (show (row!c11) ++ " " ++ show (row!c12))) rs

--
-- Testing db layout functions
--

listTables db = tables db >>= putStr . unlines

-- run 'describe'
describeTable table db = describe db table >>= putStr . unlines . map show

bigTest db = do
	     putStrLn "Tables:"
	     listTables db
	     cols <- describe db "test_tb1"
	     putStrLn "Columns in test_tb1"
	     putStrLn (unlines (map show cols))
	     putStrLn "Contents of test_tb1"
	     res <- query db q 
	     printResults res
	     (x::Int) <- randomIO
	     (y::Int) <- randomIO
	     let my = if even y then Just y else Nothing
--	     insertNew db test_tb1 (newRec x my)
--	     insert db test_tb1 (project (newRec x my))
--	     putStrLn $ "Contents of test_tb1 after inserting " ++ show (x,my)

	     putStrLn "Contents of test_tb1"
	     res <- query db q 
	     printResults res

main = argConnect bigTest
       
