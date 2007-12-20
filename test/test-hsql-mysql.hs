import Database.HaskellDB.HSQL.MySQL

import RunTests

opts = []

main = dbTestMain $ Conn {
                           dbLabel = "hsql-mysql",
                           dbConn = connect driver opts
                         }
