-- Nasty hack, demonstatrates how to use HaskellDB's internals to
-- access non-standard database features.

import Database.HaskellDB
import Database.HaskellDB.DBLayout
import Database.HaskellDB.Database
import Database.HaskellDB.Query
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Sql hiding (tables)

import TestConnect

import Data.Maybe
import System.Time

now :: Expr CalendarTime
now = Expr (ConstExpr (OtherLit "NOW()"))

data Timefield = Timefield
instance FieldTag Timefield where fieldName _ = "timefield"
timefield = mkAttr Timefield :: Attr Timefield CalendarTime

q = project (timefield << now)

getTime :: Database -> IO CalendarTime
getTime db = do
	     (r:_) <- query db q
	     return (r!timefield)

printTime db = do
	       putStrLn $ show $ showSql q
	       t <- getTime db
	       putStrLn $ calendarTimeToString t

main = argConnect printTime