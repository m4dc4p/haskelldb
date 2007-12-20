import Database.HaskellDB.HDBC.SQLite3

import RunTests

opts = [("filepath","hdbc-sqlite3-test.db")]

main = dbTestMain $ Conn {
                           dbLabel = "hdbc-sqlite3",
                           dbConn = connect driver opts
                         }
