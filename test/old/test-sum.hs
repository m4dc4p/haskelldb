import Database.HaskellDB

import TestConnect

import Dp037.D3proj_time_reports


q   = do
      reports <- table d3proj_time_reports
      project (userid << reports!userid 
	      # xid << _sum (reports!xid))

test db 
    = do
      result <- query db q
      mapM_ (putStrLn . show) result

main = do
       putStrLn $ show $ showSql q
       argConnect test
