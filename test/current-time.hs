import Database.HaskellDB
import Database.HaskellDB.HDBRec
import Database.HaskellDB.HDBRecUtils
import Database.HaskellDB.Database
import Database.HaskellDB.Query
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Sql hiding (tables)

import TestConnect

import Data.Maybe
import System.Time

q = Project [("t",BinExpr (OpOther "NOW()") (ConstExpr "") (ConstExpr ""))] Empty

data Timefield = Timefield
instance HDBRecEntry Timefield (Expr CalendarTime)
instance FieldTag Timefield where fieldName _ = "timefield"
timefield :: Attr Timefield r CalendarTime
timefield = mkAttr Timefield

getTime db = do
	     (r:_) <- dbQuery db q (Rel 0 ["timefield"]::Rel (HDBRecCons Timefield CalendarTime HDBRecTail))
	     return (r!.timefield)
	     
printTime db = do
	       putStrLn $ show $ ppSql $ toSql q
	       t <- getTime db
	       putStrLn $ calendarTimeToString t

main = argConnect printTime