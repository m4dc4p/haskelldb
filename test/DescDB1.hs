module Main where

import Database.HaskellDB.DBLayout
import Database.HaskellDB.DBSpec.DBSpecToDBDirect
import Database.HaskellDB.DBSpec.PPHelpers (mkIdentPreserving)

tables = [
          string_tbl,
          int_tbl,
          integer_tbl,
          double_tbl,
          bool_tbl,
          calendartime_tbl,
          t1,
          t2
         ]

string_tbl = TInfo {tname = "string_tbl",
	    cols = [
                    CInfo {cname = "f01", descr = (StringT,       True )},
		    CInfo {cname = "f02", descr = (StringT,       False)},
		    CInfo {cname = "f03", descr = (StringT,       True )},
		    CInfo {cname = "f04", descr = (StringT,       False)}
                   ]}

int_tbl = TInfo {tname = "int_tbl",
	    cols = [
                    CInfo {cname = "f01", descr = (IntT,       True )},
		    CInfo {cname = "f02", descr = (IntT,       False)},
		    CInfo {cname = "f03", descr = (IntT,       True )},
		    CInfo {cname = "f04", descr = (IntT,       False)}
                   ]}


integer_tbl = TInfo {tname = "integer_tbl",
	    cols = [
                    CInfo {cname = "f01", descr = (IntegerT,       True )},
		    CInfo {cname = "f02", descr = (IntegerT,       False)},
		    CInfo {cname = "f03", descr = (IntegerT,       True )},
		    CInfo {cname = "f04", descr = (IntegerT,       False)}
                   ]}

double_tbl = TInfo {tname = "double_tbl",
	    cols = [
                    CInfo {cname = "f01", descr = (DoubleT,       True )},
		    CInfo {cname = "f02", descr = (DoubleT,       False)},
		    CInfo {cname = "f03", descr = (DoubleT,       True )},
		    CInfo {cname = "f04", descr = (DoubleT,       False)}
                   ]}

bool_tbl = TInfo {tname = "bool_tbl",
	    cols = [
                    CInfo {cname = "f01", descr = (BoolT,       True )},
		    CInfo {cname = "f02", descr = (BoolT,       False)},
		    CInfo {cname = "f03", descr = (BoolT,       True )},
		    CInfo {cname = "f04", descr = (BoolT,       False)}
                   ]}

calendartime_tbl = TInfo {tname = "calendartime_tbl",
	    cols = [
                    CInfo {cname = "f01", descr = (CalendarTimeT,       True )},
		    CInfo {cname = "f02", descr = (CalendarTimeT,       False)},
		    CInfo {cname = "f03", descr = (CalendarTimeT,       True )},
		    CInfo {cname = "f04", descr = (CalendarTimeT,       False)}
                   ]}


t1 = TInfo {tname = "hdb_t1",
	    cols = [CInfo {cname = "t1f01", descr = (StringT,       True )},
		    CInfo {cname = "t1f02", descr = (StringT,       False)},
		    CInfo {cname = "t1f03", descr = (StringT,       True )},
		    CInfo {cname = "t1f04", descr = (StringT,       False)},

		    CInfo {cname = "t1f05", descr = (IntT,          True )},
		    CInfo {cname = "t1f06", descr = (IntT,          False)},
		    CInfo {cname = "t1f07", descr = (IntT,          True )},
		    CInfo {cname = "t1f08", descr = (IntT,          False)},

		    CInfo {cname = "t1f09", descr = (IntegerT,      True )},
		    CInfo {cname = "t1f10", descr = (IntegerT,      False)},
		    CInfo {cname = "t1f11", descr = (IntegerT,      True )},
		    CInfo {cname = "t1f12", descr = (IntegerT,      False)},

		    CInfo {cname = "t1f13", descr = (DoubleT,       True )},
		    CInfo {cname = "t1f14", descr = (DoubleT,       False)},
		    CInfo {cname = "t1f15", descr = (DoubleT,       True )},
		    CInfo {cname = "t1f16", descr = (DoubleT,       False)},
                    
-- Disabled for now, since booleans don't really work anywhere
--		    CInfo {cname = "t1f17", descr = (BoolT,         True )},
--		    CInfo {cname = "t1f18", descr = (BoolT,         False)},
--		    CInfo {cname = "t1f19", descr = (BoolT,         True )},
--		    CInfo {cname = "t1f20", descr = (BoolT,         False)},

		    CInfo {cname = "t1f21", descr = (CalendarTimeT, True )},
		    CInfo {cname = "t1f22", descr = (CalendarTimeT, False)},
		    CInfo {cname = "t1f23", descr = (CalendarTimeT, True )},
		    CInfo {cname = "t1f24", descr = (CalendarTimeT, False)}
                   ]}

t2 = TInfo {tname = "hdb_t2",
	    cols = [CInfo {cname = "t2f01", descr = (StringT,       True )},
		    CInfo {cname = "t2f02", descr = (StringT,       False)},
		    CInfo {cname = "t2f03", descr = (StringT,       True )},
		    CInfo {cname = "t2f04", descr = (StringT,       False)},

		    CInfo {cname = "t2f05", descr = (IntT,          True )},
		    CInfo {cname = "t2f06", descr = (IntT,          False)},
		    CInfo {cname = "t2f07", descr = (IntT,          True )},
		    CInfo {cname = "t2f08", descr = (IntT,          False)},

		    CInfo {cname = "t2f09", descr = (IntegerT,      True )},
		    CInfo {cname = "t2f10", descr = (IntegerT,      False)},
		    CInfo {cname = "t2f11", descr = (IntegerT,      True )},
		    CInfo {cname = "t2f12", descr = (IntegerT,      False)},

		    CInfo {cname = "t2f13", descr = (DoubleT,       True )},
		    CInfo {cname = "t2f14", descr = (DoubleT,       False)},
		    CInfo {cname = "t2f15", descr = (DoubleT,       True )},
		    CInfo {cname = "t2f16", descr = (DoubleT,       False)},

-- Disabled for now, since booleans don't really work anywhere
--		    CInfo {cname = "t2f17", descr = (BoolT,         True )},
--		    CInfo {cname = "t2f18", descr = (BoolT,         False)},
--		    CInfo {cname = "t2f19", descr = (BoolT,         True )},
--		    CInfo {cname = "t2f20", descr = (BoolT,         False)},

		    CInfo {cname = "t2f21", descr = (CalendarTimeT, True )},
		    CInfo {cname = "t2f22", descr = (CalendarTimeT, False)},
		    CInfo {cname = "t2f23", descr = (CalendarTimeT, True )},
		    CInfo {cname = "t2f24", descr = (CalendarTimeT, False)}
                   ]}


createModules :: IO ()
createModules = dbInfoToModuleFiles "." "DB1" hdb_test_db
  where hdb_test_db = 
          DBInfo {dbname = "hdb_test_db"
                 , opts = DBOptions {useBString = False, makeIdent = mkIdentPreserving}
                 , tbls = tables}


    

main :: IO ()
main = createModules
