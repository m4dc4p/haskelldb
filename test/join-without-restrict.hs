-- demonstrates an old bug with joins without a restrict

import Database.HaskellDB
import Database.HaskellDB.Sql
import Database.HaskellDB.Query
import Database.HaskellDB.Optimize
import Database.HaskellDB.HDBRec
import Database.HaskellDB.PrimQuery

import TestConnect

import Dp037.D3proj_time_reports hiding (xid)
import qualified Dp037.D3proj_time_reports
import Dp037.D3proj_users

-- join and project, but no restrict
q1 = do
    reports <- table d3proj_time_reports
    users <- table d3proj_users
    project (userid << reports!userid
	     # first_name << users!first_name 
	     # last_name << users!last_name
	     # activity << reports!activity)


-- has restrict, but does not join on any fields
q2 = do
     r <- q1
     restrict (r!userid .==. constant "d00bring")
     return r

showRow :: ShowRecRow r => r -> String
showRow = unwords . map (($ "") . snd) . showRecRow

showUnoptSql = ppSql . toSql . runQuery

testQuery db q = do
		 putStrLn "-- PrimQuery:"
		 putStrLn $ show $ showQ q
		 putStrLn "-- Optimized PrimQuery:"
		 putStrLn $ show $ showOpt q
		 putStrLn "-- SQL:"
		 putStrLn $ show $ showUnoptSql q
		 putStrLn "-- Optimized SQL:"
		 putStrLn $ show $ showSql q
		 rs <- query db q
		 mapM_ (putStrLn . showRow) rs

test db = do
	  testQuery db q1
	  testQuery db q2

main = argConnect test
