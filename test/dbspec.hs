module Main where

import Database.HaskellDB
import Database.HaskellDB.FieldType
import Database.HaskellDB.DBSpec
import Database.HaskellDB.DBSpec.DBSpecToDatabase

import System.Environment (getArgs)

test = DBInfo {dbname = "ctest", opts = testopts, tbls = [testtbl1,testtbl2]}

testopts = DBOptions {useBString = False}

testtbl1 = TInfo {tname = "tokenfatgirls", cols = [testcol11,testcol12]}
testtbl2 = TInfo {tname = "boysnthahood", cols = [testcol21,testcol22]}

testcol11 = CInfo {cname = "Karin", descr = (IntT,False)}
testcol12 = CInfo {cname = "DinMamma", descr = (BStrT 8,True)}

testcol21 = CInfo {cname = "Karin", descr = (BStrT 6,False)}
testcol22 = CInfo {cname = "DinMamma", descr = (IntT,True)}

main = do 
       args <- getArgs
       let db = genericConnect (head args) (tail args)
       db (dbSpecToDatabase test)
