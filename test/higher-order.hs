import Database.HaskellDB

import TestConnect

import Dp037.D3proj_time_reports hiding (xid)
import qualified Dp037.D3proj_time_reports
import Dp037.D3proj_users

onReportsUsers f
    = do
      reports <- table d3proj_time_reports
      users <- table d3proj_users
      restrict (reports!userid .==. users!xid)
      r <- project (userid << users!xid
		    # first_name << users!first_name
		    # last_name << users!last_name
		    # email << users!email
		    # day << reports!day
		    # hours << reports!hours
		    # activity << reports!activity
		    # reported << reports!reported)
      f r

q1 r = do
       restrict (r!userid .==. constant "d00bring")
       return r

{-
-- shouldn't work, and with the new type system restrictions on 
-- where aggregate functions can be used, it doesn't
q2 r = do
       u <- project (userid << r!userid # hours << r!hours)
       restrict (_sum (u!hours) .>. constant 100.0)
       return 
-}

q3 r = do
       u <- project (userid << r!userid # hours << _sum(r!hours))
       restrict (u!hours .>. constant 12.0)
       return u

test db = 
    do
    rs1 <- query db $ onReportsUsers q1
    mapM_ (putStrLn . showRow) rs1
--    putStrLn $ show $ showQ $ onReportsUsers q2
--    putStrLn $ show $ showOpt $ onReportsUsers q2
--    putStrLn $ show $ showSql $ onReportsUsers q2
--    rs2 <- query db $ onReportsUsers q2
--    mapM_ (putStrLn . showRow) rs2
    putStrLn $ show $ showQ $ onReportsUsers q3
    putStrLn $ show $ showOpt $ onReportsUsers q3
    putStrLn $ show $ showSql $ onReportsUsers q3
    rs3 <- query db $ onReportsUsers q3
    putStrLn ">100:"
    mapM_ (putStrLn . showRow) rs3
    

showRow r = r!userid ++ ": " ++ show (r!hours)

main = argConnect test
