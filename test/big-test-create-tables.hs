--
-- WARNING! This program will DROP the tables hdb_test_tb1 and hdb_test_tb2
--


import Database.HSQL.ODBC as OD
import Database.HSQL.MySQL as MY
import Database.HSQL.PostgreSQL as PG

import Data.List
import Control.Monad
import System.Environment

data Table = Table String [(String,String)]

t1 = Table "hdb_test_t1" [
			  ("t1f01","char(8)"),
			  ("t1f02","char(8) not null"),
			  ("t1f03","varchar(8)"),
			  ("t1f04","varchar(8) not null"),
			  ("t1f05","smallint"),
			  ("t1f06","smallint not null"),
			  ("t1f07","numeric"),
			  ("t1f08","numeric not null"),
			  ("t1f09","decimal"),
			  ("t1f10","decimal not null"),
			  ("t1f11","int"),
			  ("t1f12","int not null"),
			  ("t1f13","float"),
			  ("t1f14","float not null"),
			  ("t1f15","real"),
			  ("t1f16","real not null"),
			  ("t1f17","double precision"),
			  ("t1f18","double precision not null"),
			  ("t1f19","date"),
			  ("t1f20","date not null")
-- Insertion doesn't work in Postgre
--			  ("t1f21","time"),
--			  ("t1f22","time not null"),
-- Insertion doesn't work in Postgre
--			  ("t1f23","timestamp"),
--			  ("t1f24","timestamp not null")
			 ]

t2 = Table "hdb_test_t2" [
			  ("t2f01","char(8)"),
			  ("t2f02","char(8) not null"),
			  ("t2f03","varchar(8)"),
			  ("t2f04","varchar(8) not null"),
			  ("t2f05","smallint"),
			  ("t2f06","smallint not null"),
			  ("t2f07","numeric"),
			  ("t2f08","numeric not null"),
			  ("t2f09","decimal"),
			  ("t2f10","decimal not null"),
			  ("t2f11","int"),
			  ("t2f12","int not null"),
			  ("t2f13","float"),
			  ("t2f14","float not null"),
			  ("t2f15","real"),
			  ("t2f16","real not null"),
			  ("t2f17","double precision"),
			  ("t2f18","double precision not null"),
			  ("t2f19","date"),
			  ("t2f20","date not null")
-- Insertion doesn't work in Postgre
--			  ("t2f21","time"),
--			  ("t2f22","time not null"),
-- Treated as strings in Postgre
--			  ("t2f23","timestamp"),
--			  ("t2f24","timestamp not null")
			 ]

tableName :: Table -> String
tableName (Table n _) = n

mkDrop (Table n fs) = "DROP TABLE " ++ n
mkCreate (Table n fs) = "CREATE TABLE " ++ n ++ " ( " 
			++ concat (intersperse "," 
				   (map (\ (f,t) -> f ++ " " ++ t) fs))
			++ ")"

conn ["ODBC",d,u,p] = OD.connect d u p
conn ["MySQL",h,d,u,p] = MY.connect h d u p
conn ["PostgreSQL",h,d,u,p] = PG.connect h d u p
conn _ = fail $ unlines ["Accepted options:",
			     "ODBC dsn uid pwd",
			     "MySQL server db uid pwd",
			     "PostgreSQL server db uid pwd"]


createTables db [] = return ()
createTables db (t:ts) = 
    do
    putStrLn $ "Creating " ++ tableName t
    handleSql (\_ -> dropCreate) (execute db c)
    createTables db ts
	where c = mkCreate t
	      dropCreate = do
			   putStrLn $ "Dropping " ++ tableName t
			   execute db (mkDrop t)
			   putStrLn $ "Creating " ++ tableName t
			   execute db c

run args = do
	   db <- conn args
	   createTables db [t1, t2]
	   disconnect db

main = do
       args <- getArgs
       handleSql (fail . show) (run args)
