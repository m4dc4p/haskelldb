import System.Time (calendarTimeToString)

import Database.HaskellDB
import Dp037.D3proj_time_reports hiding (xid)
import Dp037.D3proj_users

import TestConnect

getUsers = 
    do
    users <- table d3proj_users
    reports <- table d3proj_time_reports
    order [asc reports day]
    restrict (users!xid .==. reports!userid)
    project (first_name << users!first_name # 
	     last_name << users!last_name # 
	     day << reports!day #
	     reported << reports!reported
	     )


showReport r = rpad 20 (r!first_name ++ " " ++ r!last_name) ++ " " 
	       ++ calendarTimeToString (r!day) ++ " "
	       ++ calendarTimeToString (r!reported)

rpad :: Int -> String -> String
rpad x s = s ++ replicate (x - length s) ' '

printReports db = 
    do
    users <- query db getUsers
    mapM_ (putStrLn . showReport) users

main = argConnect printReports
