{-
  A very disorganized set of tests using almost all SQL92 data types.
-}

import System.Time
import System.Locale
import System.IO.Unsafe

import Database.HaskellDB
import Database.HaskellDB.HDBRec
import Database.HaskellDB.Query

import TestConnect

import BigTestDB.Hdb_test_t1
import BigTestDB.Hdb_test_t2

now = unsafePerformIO (getClockTime >>= toCalendarTime)


data1 = [(
	 t1f01 << constant (Just "bepa") #
	 t1f02 << constant "apa" #
	 t1f03 << constant (Just "bar")	#
	 t1f04 << constant "foo" #
	 t1f05 << constant (Just 2) #
	 t1f06 << constant 1 #
	 t1f07 << constant (Just 4.0) #
	 t1f08 << constant 3.0 #
	 t1f09 << constant (Just 5.0) #
	 t1f10 << constant 6.0 #
	 t1f11 << constant (Just 7) #
	 t1f12 << constant (-13) #
	 t1f13 << constant (Just 3.1415) #
	 t1f14 << constant (-3456.8) #
	 t1f15 << constant (Just (-67.5)) #
	 t1f16 << constant 12.0 #
	 t1f17 << constant (Just 17.17) #
	 t1f18 << constant 18.18 #
	 t1f19 << constant (Just now) #
	 t1f20 << constant now #
-- Insertion doesn't work in Postgre
--	 t1f21 << constant (Just now) #
--	 t1f22 << constant now #
-- MySQL makes this NOT NULL
--	 t1f23 << constant now #
-- Treated as strings in Postgre
	 t1f23 << constant (Just now) #
	 t1f24 << constant now
	)
	]

data2 =  [(
	 t2f01 << constant (Just "bepa") #
	 t2f02 << constant "mepa" #
	 t2f03 << constant (Just "sdfjhc")	#
	 t2f04 << constant "quasar" #
	 t2f05 << constant (Just 2) #
	 t2f06 << constant 1 #
	 t2f07 << constant (Just 4.0) #
	 t2f08 << constant 3.0 #
	 t2f09 << constant (Just 5.0) #
	 t2f10 << constant 6.0 #
	 t2f11 << constant (Just 7) #
	 t2f12 << constant (-13) #
	 t2f13 << constant (Just 3.1415) #
	 t2f14 << constant (-3456.8) #
	 t2f15 << constant (Just (-67.5)) #
	 t2f16 << constant 12.0 #
	 t2f17 << constant (Just 17.17) #
	 t2f18 << constant 18.18 #
	 t2f19 << constant (Just now) #
	 t2f20 << constant now #
-- Insertion doesn't work in Postgre
--	 t2f21 << constant (Just now) #
--	 t2f22 << constant now #
-- MySQL makes this NOT NULL
--	 t2f23 << constant now #
-- Treated as strings in Postgre
	 t2f23 << constant (Just now) #
	 t2f24 << constant now
	)]

insertData db = do
		mapM (insert db hdb_test_t1) data1
		mapM (insert db hdb_test_t2) data2


deleteData db = do
		putStrLn "Deleting data from hdb_test_t1..."
		delete db hdb_test_t1 (\r -> constant True)
		putStrLn "Deleting data from hdb_test_t2..."
		delete db hdb_test_t2 (\r -> constant True)

mkJoinOnQuery f1 f2 = 
    do
    t1 <- table hdb_test_t1
    t2 <- table hdb_test_t2
    restrict (t1!f1 .==. t2!f2)
    project (f1 << t1!f1 # f2 << t2!f2 # 
	     t1f01 << t1!t1f01 # t2f01 << t2!t2f01)

joinOn db f1 f2 t1 t2 = 
    do
    rs <- query db (mkJoinOnQuery f1 f2)
    if and [r!.t1 == r!.t2 | r <- rs] then
       putStrLn "Join ok"
     else
       putStrLn "Join equality check FAILED"

formatDate = formatCalendarTime defaultTimeLocale fmt
	where fmt = iso8601DateFormat (Just "%H:%M:%S")

showAllFields1 r = [
		    show $ r!.t1f01,
		    show $ r!.t1f02,
		    show $ r!.t1f03,
		    show $ r!.t1f04,
		    show $ r!.t1f05,
		    show $ r!.t1f06,
		    show $ r!.t1f07,
		    show $ r!.t1f08,
		    show $ r!.t1f09,
		    show $ r!.t1f10,
		    show $ r!.t1f11,
		    show $ r!.t1f12,
		    show $ r!.t1f13,
		    show $ r!.t1f14,
		    show $ r!.t1f15,
		    show $ r!.t1f16,
		    show $ r!.t1f17,
		    show $ r!.t1f18,
		    show $ r!.t1f19,
		    show $ r!.t1f20
-- Insertion doesn't work in Postgre
--		    show $ r!.t1f21,
--		    show $ r!.t1f22,
-- Treated as strings in Postgre
--		    show $ r!.t1f23,
--		    show $ r!.t1f24
		   ]

showAll db = 
    do
    rs <- query db (table hdb_test_t1)
    mapM_ (putStrLn . unwords . showAllFields1) rs

q1 = do
     t <- table hdb_test_t1
     restrict (t!t1f02 .==. constant "apa")
     return t

q2 = do
     t <- table hdb_test_t1
     restrict (t!t1f10 .==. constant 6.0)
     return t

printQuery = putStrLn . show . showSql

doQuery db q =
    do
    --printQuery q
    query db q

testOps db =
    do
    putStrLn "Testing UNION..."
    query db (union q1 q2)
-- These don't work in MySQL:
--    query db (intersect q1 q2)
--    query db (divide q1 q2)
--    query db (minus q1 q2)

runTests db =
    do
    insertData db
    putStrLn "After INSERT:"
    showAll db
    joinOn db t1f01 t2f01 t1f01 t2f01
    joinOn db t1f02 t2f02 t1f02 t2f02
    joinOn db t1f03 t2f03 t1f03 t2f03
    joinOn db t1f04 t2f04 t1f04 t2f04
    joinOn db t1f05 t2f05 t1f05 t2f05
    joinOn db t1f06 t2f06 t1f06 t2f06
    testOps db    
    deleteData db
    putStrLn "After DELETE:"

main = argConnect runTests
--       putStrLn $ unlines $ [ v "" | (_,v) <- showRecRow data0]
