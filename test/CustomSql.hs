module CustomSql where

import Database.HaskellDB
import Database.HaskellDB.DBLayout
import Database.HaskellDB.Query
import Database.HaskellDB.PrimQuery
import System.Time

--
-- Fields for getting results of a given type
--

data Timefield = Timefield
instance FieldTag Timefield where fieldName _ = "timefield"
timefield = mkAttr Timefield :: Attr Timefield CalendarTime

data Intfield = Intfield
instance FieldTag Intfield where fieldName _ = "intfield"
intfield = mkAttr Intfield :: Attr Intfield Int

data Boolfield = Boolfield
instance FieldTag Boolfield where fieldName _ = "boolfield"
boolfield = mkAttr Boolfield :: Attr Boolfield Bool

--
-- Utilities
--

binop :: String -> Expr a -> Expr b -> Expr c
binop op (Expr e1) (Expr e2) = Expr (BinExpr (OpOther op) e1 e2)

--
-- Custom sql operators
--

now :: Expr CalendarTime
now = Expr (ConstExpr (OtherLit "NOW()"))

last_insert_id :: Expr Int
last_insert_id = Expr (ConstExpr (OtherLit "LAST_INSERT_ID()"))

ilike :: Expr String -> Expr String -> Expr Bool
ilike = binop "ILIKE"


