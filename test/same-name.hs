import Database.HaskellDB

import TestConnect

import qualified Dp037.D3proj_time_reports as R
import qualified Dp037.D3proj_users as U

q = do
    reports <- table R.d3proj_time_reports
    users <- table U.d3proj_users
    project (R.xid << reports!R.xid
	     # U.xid << users!U.xid)

test db = do
	  rs <- query db q
	  mapM_ (\r -> print (r!R.xid, r!U.xid)) rs

main = do
       putStrLn $ show $ showSql q
       argConnect test