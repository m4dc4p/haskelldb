import Database.HaskellDB

import TestConnect

import Dp037.D3proj_users

getUsers = 
    do
    users <- table d3proj_users
    order [asc users last_name, asc users first_name]
    project (first_name << users!first_name # 
	     last_name << users!last_name # 
	     email << users!email)


showUser u = "<li>" ++ u!first_name ++ " " ++ u!last_name 
		 ++ " (<tt>" ++ obfuscate (u!email) 
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

main = argConnect printUserList
