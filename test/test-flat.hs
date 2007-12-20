import Database.HaskellDB.FlatDB

import RunTests

opts = [("filepath","flatdb-test.db")]

main = dbTestMain $ Conn {
                           dbLabel = "flat",
                           dbConn = connect driver opts
                         }
