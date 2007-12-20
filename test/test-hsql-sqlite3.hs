import Database.HaskellDB.HSQL.SQLite3

import RunTests

opts = [("filepath","hsql-sqlite3-test.db"),("mode","rw")]

main = dbTestMain $ Conn {
                           dbLabel = "hsql-sqlite3",
                           dbConn = connect driver opts
                         }
