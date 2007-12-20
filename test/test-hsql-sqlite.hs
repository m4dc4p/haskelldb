import Database.HaskellDB.HSQL.SQLite

import RunTests

opts = [("filepath","hsql-sqlite-test.db"),("mode","rw")]

main = dbTestMain $ Conn {
                           dbLabel = "hsql-sqlite",
                           dbConn = connect driver opts
                         }
