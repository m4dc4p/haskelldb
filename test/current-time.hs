import Database.HaskellDB
import Database.HaskellDB.DBLayout
import Database.HaskellDB.Database
import Database.HaskellDB.Query
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Sql hiding (tables)

import TestConnect

import Data.Maybe
import System.Time

q = Project [("timefield",BinExpr (OpOther "NOW()") (ConstExpr "") (ConstExpr ""))] Empty

data Timefield = Timefield
instance FieldTag Timefield where fieldName _ = "timefield"
timefield :: Attr Timefield CalendarTime
timefield = mkAttr Timefield

getTime db = do
	     (r:_) <- dbQuery db q (Rel 0 ["timefield"]::Rel (HDBRecCons Timefield (Expr CalendarTime) HDBRecTail))
	     return (Row r!timefield)
	     
printTime db = do
	       putStrLn $ show $ ppSql $ toSql q
	       t <- getTime db
	       putStrLn $ calendarTimeToString t

main = argConnect printTime