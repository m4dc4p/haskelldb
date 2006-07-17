import Database.HaskellDB

import TestConnect

import Dp037.D3proj_users

-- victor said that top, topPercent and union produce SQL errors

opts = ODBCOptions{dsn="mysql-dp037", uid="dp037", pwd="teent333"}
withDB f = odbcConnect opts f

q1 = do
    users <- table d3proj_users
    top 2
    order [asc users xid]
    return users

{-
q2 = do
    users <- table d3proj_users
    topPercent 20
    return users
-}

q3 = do
    users <- table d3proj_users
    order [desc users xid]
    top 2
    return users

pp = putStrLn . show . showSql

printIds = mapM (\r -> putStrLn (r!xid))

tests db = do 
	   putStrLn "top:"
	   pp q1
	   rs <- query db q1
	   printIds rs
	   putStrLn ""
--       putStrLn "topPercent:"
--       pp q2
--	   putStrLn ""
	   putStrLn "union:"
	   let u = q1 `union` q3
	   pp u
	   rs <- query db u
	   printIds rs
	   putStrLn ""

main = argConnect tests
