module TestCases where

import DB1.Hdb_t1

import Database.HaskellDB

import DBTest

import Test.HUnit


tests = dbtests [dbtest "deleteEmpty" testDeleteEmpty]



testDeleteEmpty db =
    do delete db hdb_t1 (\_ -> constant True)
       rs <- query db $ table hdb_t1
       assertBool "Query after complete delete is non-empty" (null rs)
