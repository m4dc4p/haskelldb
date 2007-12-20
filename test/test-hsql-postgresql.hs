import Database.HaskellDB.HSQL.PostgreSQL

import RunTests

opts = [("server", "localhost"),
        ("db","hdb_test"),
        ("uid","hdb_test"),
        ("pwd","hdb_test_pass")]

main = dbTestMain $ Conn {
                           dbLabel = "hsql-postgresql",
                           dbConn = connect driver opts
                         }
