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

q :: PrimQuery
q = Project [("timefield", ConstExpr "NOW()")] Empty

rel :: Rel (HDBRecCons Timefield (Expr CalendarTime) HDBRecTail)
rel = Rel 0 ["timefield"]

data Timefield = Timefield
instance FieldTag Timefield where fieldName _ = "timefield"
timefield = mkAttr Timefield :: Attr Timefield CalendarTime

getTime :: Database -> IO CalendarTime
getTime db = do
	     (r:_) <- dbQuery db q rel
	     return (Row r!timefield)

printTime db = do
	       putStrLn $ show $ ppSql $ toSql q
	       t <- getTime db
	       putStrLn $ calendarTimeToString t

main = argConnect printTime