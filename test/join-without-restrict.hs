-- demonstrates a bug with joins without a restrict

import Database.HaskellDB
import Database.HaskellDB.HDBRec

import TestConnect

import Dp037.D3proj_time_reports hiding (xid)
import qualified Dp037.D3proj_time_reports
import Dp037.D3proj_users

q1 = do
    reports <- table d3proj_time_reports
    users <- table d3proj_users
    project (userid << reports!userid
	     # first_name << users!first_name 
	     # last_name << users!last_name
	     # activity << reports!activity)

q2 = do
     r <- q1
     restrict (r!userid .==. constant "d00bring")
     return r

showRow = unwords . map (($ "") . snd) . showRecRow

testQuery db q = do
		 putStrLn $ show $ showSql q
		 rs <- query db q
		 mapM_ (putStrLn . showRow) rs

test db = do
	  testQuery db q2
	  testQuery db q1

main = argConnect test
