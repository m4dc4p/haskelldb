import HaskellDB
import HSQL_ODBC

import Dp037
import Dp037.D3proj_users

opts = ODBCOptions { dsn = "mysql-dp037", uid = "dp037", pwd = "teent333" }
withDB = odbcConnect opts

getUsers = 
    do
    users <- table d3proj_users
    order [asc users last_name, asc users first_name]
    project (first_name << users!first_name # 
	     last_name << users!last_name # 
	     email << users!email)


showUser u = "<li>" ++ u!.first_name ++ " " ++ u!.last_name 
		 ++ " (<tt>" ++ obfuscate (u!.email) 
			++ "</tt>)</li>"

obfuscate addr = name ++ " AT " ++ safeTail domain 
    where 
    (name,domain) = break (=='@') addr
    safeTail [] = []
    safeTail (_:xs) = xs

printUserList db = 
    do
    users <- query db getUsers
    putStrLn "<ul>"
    mapM_ (putStrLn . showUser) users
    putStrLn "</ul>"

main = withDB printUserList