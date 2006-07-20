module DBTest where

import Database.HaskellDB
import Database.HaskellDB.Database
import Database.HaskellDB.DBLayout
import Database.HaskellDB.DBSpec.DBSpecToDatabase 
import Database.HaskellDB.DynConnect

import Control.Exception (bracket_)

import Test.HUnit


data Conn = Conn {
                  dbLabel :: String,
                  dbPackage :: String,
                  dbModule :: String,
                  dbOptions :: [(String,String)]
                 }
        deriving (Show,Read)

type DBTest = DBInfo -> [Conn] -> Test

dbtests :: [DBTest] -> DBTest
dbtests fs dbi cs = TestList $ map (\f -> f dbi cs) fs

dbtest :: String -> (Database -> Assertion) -> DBTest
dbtest l f dbi cs = TestLabel l $ TestList $ map (testWithDB f dbi) cs

label :: String -> DBTest -> DBTest
label l f dbi cs = TestLabel l (f dbi cs)

testWithDB :: (Database -> Assertion) -> DBInfo -> Conn -> Test
testWithDB f dbi c = TestLabel (dbLabel c) $ TestCase $ withDB (withTables f dbi) c

withDB :: (Database -> IO a) -> Conn -> IO a
withDB f db = dynConnect (dbPackage db) (dbModule db) (dbOptions db) f

withTables :: (Database -> IO a) -> DBInfo -> Database -> IO a
withTables f dbi db = bracket_ create drop (f db)
  where create = do mapM_ (dropIfExists db . tname) ts
                    mapM_ (tInfoToTable db) ts
        drop   = mapM_ (dropTable db . tname) ts
        ts     = tbls dbi
        

dropIfExists :: Database -> String -> IO ()
dropIfExists db t = do ts <- tables db
                       if t `elem` ts then dropTable db t else return ()
