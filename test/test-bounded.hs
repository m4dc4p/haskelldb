import System.Environment

import Database.HaskellDB
import Database.HaskellDB.BoundedList
import Database.HaskellDB.HSQL.ODBC
import Dp037_bounded.D3proj_users

opts = ODBCOptions{dsn="mysql-dp037", uid="dp037", pwd="teent333"}
withDB f = odbcConnect opts f

q n = do
      users <- table d3proj_users
      restrict (users!xid .==. constant (trunc n))
      project (first_name << users!first_name # last_name << users!last_name)

printName r = putStrLn (fromBounded (r!.first_name) ++ " " 
			++ fromBounded (r!.last_name))	      

main = do
       [user] <- getArgs
       withDB (\db -> query db (q user) >>= mapM_ printName) 