import Database.HaskellDB

import TestConnect
import THField

import Dp037.D3proj_time_reports

$(field "_foo" "foo" "Foo" False "Int")

q = do
    t <- table d3proj_time_reports
    project (_foo << count(t!userid))

test db = do
	  rs <- query db q
	  print rs

main = argConnect test