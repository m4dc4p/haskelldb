import System (getArgs)
import Control.Monad (unless)

import Database.HaskellDB
import Dp037.D3proj_time_reports hiding (xid)
import qualified Dp037.D3proj_time_reports
import Dp037.D3proj_users
--import Database.HaskellDB.HSQL.PostgreSQL
import Database.HaskellDB.HSQL.ODBC

opts = ODBCOptions { dsn = "mysql-dp037", uid = "dp037", pwd = "teent333" }
withDB = odbcConnect opts

getUsers = 
    do
    users <- table d3proj_users
    reports <- table d3proj_time_reports
    order [asc reports day]
    restrict (users!xid .==. reports!userid)
    project (first_name << users!first_name # 
	     last_name << users!last_name # 
	     day << reports!day)


showReport r = r!.first_name ++ " " ++ r!.last_name ++ " " 
	       ++ show (r!.day)

printReports db = 
    do
    users <- query db getUsers
    mapM_ (putStrLn . showReport) users

main = withDB printReports