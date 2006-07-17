import System.Environment

import TestConnect

import Database.HaskellDB
import Database.HaskellDB.BoundedList
import Database.HaskellDB.HSQL.ODBC
import Dp037_bounded.D3proj_users

q n = do
      users <- table d3proj_users
      restrict (users!xid .==. constant (trunc n))
      project (first_name << users!first_name # last_name << users!last_name)

printName r = putStrLn (fromBounded (r!first_name) ++ " " 
			++ fromBounded (r!last_name))	      

main = do
       connArgs <- getArgs
       connect connArgs (\db -> query db (q "d00bring") >>= mapM_ printName) 