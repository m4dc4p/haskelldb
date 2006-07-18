module DBTest where


import Database.HaskellDB
import Database.HaskellDB.DynConnect

import Test.HUnit


data DB = DB {
              dbLabel :: String,
              dbPackage :: String,
              dbModule :: String,
              dbOptions :: [(String,String)]
             }
        deriving (Show,Read)

dbtests :: [[DB] -> Test] -> [DB] -> Test
dbtests fs dbs = TestList $ map ($ dbs) fs

dbtest :: String -> (Database -> Assertion) -> [DB] -> Test
dbtest l f dbs = TestLabel l $ TestList $ map (testWithDB f) dbs

testWithDB :: (Database -> Assertion) -> DB -> Test
testWithDB f db = TestLabel (dbLabel db) $ TestCase $ withDB f db

withDB :: (Database -> IO a) -> DB -> IO a
withDB f db = dynConnect (dbPackage db) (dbModule db) (dbOptions db) f
