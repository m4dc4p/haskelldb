{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
module DBTest where

import Database.HaskellDB
import Database.HaskellDB.Database
import Database.HaskellDB.DBLayout
import Database.HaskellDB.DBSpec.DBSpecToDatabase 

import Control.Exception (bracket_)

import Test.HUnit

data Conn = Conn {
                   dbLabel :: String,
                   dbConn :: forall a. (Database -> IO a) -> IO a
    }

type DBTest = DBInfo -> Conn -> Test

dbtests :: [DBTest] -> DBTest
dbtests fs dbi c = TestList $ map (\f -> f dbi c) fs

dbtest :: String -> (Database -> Assertion) -> DBTest
dbtest l f dbi c = TestLabel l $ testWithDB f dbi c

label :: String -> DBTest -> DBTest
label l f dbi c = TestLabel l (f dbi c)

testWithDB :: (Database -> Assertion) -> DBInfo -> Conn -> Test
testWithDB f dbi c = TestCase $ withDB (withTables f dbi) c

withDB :: (Database -> IO a) -> Conn -> IO a
withDB f db = dbConn db f

withTables :: (Database -> IO a) -> DBInfo -> Database -> IO a
withTables f dbi db = bracket_ create (return ()) (f db)
  where create = do mapM_ (dropIfExists db . tname) ts
                    mapM_ (tInfoToTable db) ts
                    
        drop   = mapM_ (dropIfExists db . tname) ts
        ts     = tbls dbi
        

dropIfExists :: Database -> String -> IO ()
dropIfExists db t = do ts <- tables db
                       if t `elem` ts then dropTable db t else return ()
