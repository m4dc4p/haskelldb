import Database.HaskellDB
import Database.HaskellDB.Database
import Database.HaskellDB.DBSpec.DBSpecToDatabase

import TestConnect

import Control.Exception
import System.IO

--
-- Database description
--

import Database.HaskellDB.DBLayout


dbinfo :: DBInfo
dbinfo = DBInfo {dbname = "Dp037",
                 opts = DBOptions {useBString = False},
                 tbls = [TInfo {tname = "test_non_ascii",
                               cols = [CInfo {cname = "str_id",
                                              descr = (IntT, False)},
                                       CInfo {cname = "str",
                                              descr = (StringT, False)}]}]
		}

---------------------------------------------------------------------------
-- Table
---------------------------------------------------------------------------
test_non_ascii :: Table
    ((RecCons Str_id (Expr Int)
      (RecCons Str (Expr String) RecNil)))

test_non_ascii = baseTable "test_non_ascii" $
                 hdbMakeEntry Str_id #
                 hdbMakeEntry Str

---------------------------------------------------------------------------
-- Fields
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Str_id Field
---------------------------------------------------------------------------

data Str_id = Str_id

instance FieldTag Str_id where fieldName _ = "str_id"

str_id :: Attr Str_id Int
str_id = mkAttr Str_id

---------------------------------------------------------------------------
-- Str Field
---------------------------------------------------------------------------

data Str = Str

instance FieldTag Str where fieldName _ = "str"

str :: Attr Str String
str = mkAttr Str



--
-- Database creation
--

printIOErrors :: IO () -> IO ()
printIOErrors = handleJust ioErrors (hPutStrLn stderr . show)

recreateDB :: Database -> DBInfo -> IO ()
recreateDB db info = do
		     dropTables db info
		     dbSpecToDatabase db info

dropTables :: Database -> DBInfo -> IO ()
dropTables db info = mapM_ (printIOErrors . dropTable db . tname) (tbls info)

--
-- Testing
--

addGet db i s = do
		insert db test_non_ascii (str_id << constant i 
					  # str << constant s)
		(r:_) <- query db $ do
				    t <- table test_non_ascii
				    restrict (t!str_id .==. constant i)
				    return t
		return (r!str)

showStr s = s ++ " (" ++ show s ++ ")"

testStr db i s = do
		 s' <- addGet db i s
		 putStrLn $ showStr s ++ " => " ++ showStr s'

test :: Database -> IO ()
test db = do
	  testStr db 0 "והצס"
	  testStr db 1 "'\"\\"

--
-- Main function
--

main = argConnect $ \db -> do
			   recreateDB db dbinfo
			   test db
			   dropTables db dbinfo
