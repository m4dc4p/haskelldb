import Database.HaskellDB
import Database.HaskellDB.Database
import Database.HaskellDB.Query
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Sql hiding (tables)

import Database.HaskellDB.HSQL.ODBC

import Data.Maybe
import System.Time

opts = ODBCOptions{dsn="", uid="", pwd=""}
withDB f = odbcConnect opts f

q = Project [("t",BinExpr (OpOther "NOW()") (ConstExpr "") (ConstExpr ""))] Empty

getTime db = do
	     (r:_) <- (dbQuery db) (database db) q undefined
	     return (fromJust (r!.(Attr "t")) :: CalendarTime)
	     
printTime db = do
	       t <- getTime db
	       putStrLn $ calendarTimeToString t

main = withDB printTime