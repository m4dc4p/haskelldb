import HSQL_driver
import HaskellDB

import Database.ODBC.HSQL hiding (query)
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
     (HDBRecSep (C12 (Expr Int)) HDBRecTail))

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

c11_ :: ShowRecRow r => Rel r -> Attr r a -> b -> HDBRecSep (C11 (Expr a)) b
c11_ t f = HDBRecSep $ C11 (t!f)

-------------------------------------
-- C12 Field
-------------------------------------

data C12 a = C12 a deriving Show

instance HDBRecEntry C12 (Expr Int) where
    fieldName _ = "c12"
    fieldValue (C12 x) = x

class HasC12 r
instance HasC12 (HDBRecSep (C12 a) b)
instance HasC12 b => HasC12 (HDBRecSep a b)

c12 :: HasC12 r => Attr r Int
c12 = Attr "c12"

c12_ :: ShowRecRow r => Rel r -> Attr r a -> b -> HDBRecSep (C12 (Expr a)) b
c12_ t f = HDBRecSep $ C12 (t!f)


exError :: SqlError -> IO a
exError = error . seErrorMsg

opts = ODBCOptions{dsn="mysql-dp037", uid="dp037", pwd="teent333"}

q = do
    tb1 <- table test_tb1
    hdbProject (c11_ tb1 c11 # c12_ tb1 c12)

ins = hdbMakeRec $ HDBRecSep (C11 (constant 42)) 
                   # HDBRecSep (C12 (constant 7))

-- weird type signature required by Hugs
handleQuery :: (HasC11 r, HasC12 r, Row row Int, Row row String) => [row r] -> IO ()
handleQuery = mapM_ (\row -> putStrLn (show (row!.c11) ++ " " ++ show (row!.c12)))

main = handleSql exError $ odbcConnect opts $ \db ->
        query db q >>= handleQuery
--       insertNew db test_tb1 ins


{-
fs :: ODBCOptions -> String -> IO [FieldDef]
fs opts t = do
	    conn <- connect (dsn opts) (uid opts) (pwd opts)
	    xs <- describe conn t
	    disconnect conn
	    return xs

main = do 
       defs <- fs opts "test_tb1"
       putStrLn $ unlines $ map show defs
-}