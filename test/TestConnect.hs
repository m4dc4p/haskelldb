{-# LINE 1 "TestConnect.pphs" #-}
module TestConnect where

import System.Environment

-- #define 1
-- #define 1
-- #define 1

import Database.HaskellDB.HSQL.Common

import qualified Database.HaskellDB.HSQL.ODBC as OD


import qualified Database.HaskellDB.HSQL.MySQL as MY


import qualified Database.HaskellDB.HSQL.PostgreSQL as PG


connect :: [String] -> (HSQL -> IO a) -> IO a

connect ["ODBC",d,u,p] = OD.odbcConnect OD.ODBCOptions 
				     {OD.dsn = d,OD.uid = u,OD.pwd = p}


connect ["MySQL",s,d,u,p] = MY.mysqlConnect MY.MySQLOptions 
			               {MY.server = s,MY.db = d, 
					MY.uid = u,MY.pwd = p}


connect ["PostgreSQL",s,d,u,p] = PG.postgresqlConnect PG.PostgreSQLOptions 
			               {PG.server = s,PG.db = d, 
					PG.uid = u,PG.pwd = p}

connect _ = error $ unlines ["Accepted options:",
			     "ODBC dsn uid pwd",
			     "MySQL server db uid pwd",
			     "PostgreSQL server db uid pwd"]

argConnect f = do
	       args <- getArgs 
	       connect args f