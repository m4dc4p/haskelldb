-- Gets the activity from the row with the highest id.

import Database.HaskellDB

import TestConnect

import Dp037.D3proj_time_reports

q = do
    t <- table d3proj_time_reports
    t' <- table d3proj_time_reports
    r <- project (xid << _max (t'!xid))
    restrict (t!xid .==. r!xid)
    project (activity << t!activity)

test db = do
	  print $ showSql q
	  rs <- query db q
	  mapM_ print rs

main = argConnect test