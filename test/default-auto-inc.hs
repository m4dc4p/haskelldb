import Database.HaskellDB
import Database.HaskellDB.HDBRec
import Database.HaskellDB.Query
import Database.HaskellDB.PrimQuery

import TestConnect

import Dp037.Test_default_auto


{-
MySQL:

CREATE TABLE test_default_auto (
  auto_col int NOT NULL auto_increment PRIMARY KEY,
  def1 int NOT NULL default '23',
  def2 int default '45',
  nodef1 int NOT NULL,
  nodef2 int NULL
)

PostgreSQL:

CREATE TABLE test_default_auto (
  auto_col serial,
  def1 int NOT NULL default '23',
  def2 int default '45',
  nodef1 int NOT NULL,
  nodef2 int NULL
)
-}


showResults rs = mapM_ (putStrLn . unwords . map (($ "") . snd) . showRecRow) rs

showTable db = query db (table test_default_auto) >>= showResults

last_insert_id :: Expr Int
last_insert_id = Expr (ConstExpr (OtherLit "last_insert_id()"))

lid_q = project (def1 << last_insert_id)

test db = do
	  putStrLn "Before:"
	  showTable db
	  insert db test_default_auto (auto_col << _default # 
				       def1 << _default # 
				       def2 << _default # 
				       -- PostgreSQL make DEFAULT == NULL,
				       -- so _default doesn't work for
				       -- nodef1
				       nodef1 << _default # 
				       nodef2 << _default)
	  putStrLn "After:"
	  showTable db
	  putStrLn $ show $ showSql lid_q
	  (r:_) <- query db lid_q
	  putStrLn $ "New id: " ++ show (r!def1)

main = argConnect test