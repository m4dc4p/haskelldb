module Main where

import Database.HaskellDB
import Database.HaskellDB.FieldType
import Database.HaskellDB.DBSpec

import System.Environment (getArgs)

test = DBInfo {dbname = "ctest", opts = testopts, tbls = [testtbl1,testtbl2]}

testopts = DBOptions {useBString = False}

testtbl1 = TInfo {tname = "ctesttbl1", cols = [testcol11,testcol12]}
testtbl2 = TInfo {tname = "ctesttbl2", cols = [testcol21,testcol22]}

testcol11 = CInfo {cname = "ctestcol11", descr = (IntT,False)}
testcol12 = CInfo {cname = "ctestcol12", descr = (BStrT 8,True)}

testcol21 = CInfo {cname = "ctestcol21", descr = (BStrT 6,False)}
testcol22 = CInfo {cname = "ctestcol22", descr = (IntT,True)}

main = do 
       args <- getArgs
       let db = genericConnect (head args) (tail args)
       db (\a -> dbSpecToDatabase a test)
