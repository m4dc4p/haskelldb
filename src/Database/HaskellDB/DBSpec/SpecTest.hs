module SpecTest where

import Database.HaskellDB.DBSpec.DBSpec
import Database.HaskellDB.FieldType
import Database.HaskellDB.DBSpec.DBSpecToDBDirect

-- politically incorrect

test = DBInfo {dbname="Chuckys Testdb", opts=DBOptions{useBString=True}, 
	       tbls=testtbls}

testtbls = [TInfo {tname="ASS-table", cols=asscols},
	    TInfo {tname="BREAST-table",cols=breastcols}]

asscols = [CInfo {cname="ass1",descr=(StringT,True)},
	   CInfo {cname="ass-TAble",descr=(IntT,False)}]

breastcols = [CInfo {cname="b1",descr=(StringT,True)},
	      CInfo {cname="ass-TAble",descr=(DoubleT,False)}]