import Database.HaskellDB.HDBC.PostgreSQL

import RunTests

opts = [("host", "localhost"),
        ("dbname","hdb_test"),
        ("user","hdb_test"),
        ("password","hdb_test_pass")]

main = dbTestMain $ Conn {
                           dbLabel = "hdbc-postgresql",
                           dbConn = connect driver opts
                         }
