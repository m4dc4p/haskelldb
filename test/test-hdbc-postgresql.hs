import Database.HaskellDB.HDBC.PostgreSQL
import Database.HDBC
import System.Console.GetOpt
import System.Environment
import System.Exit
import Control.Monad

import RunTests

data Options = Host String
            | Database String
            | User String
            | Password String
            | Help 
  deriving Eq

opts = [Option ['h'] ["host"] (ReqArg Host "host") "Host to connect to for testing."
       , Option ['d'] ["dbname"] (ReqArg Database "database") "Name of database to use for testing."
       , Option ['u'] ["user"] (ReqArg User "username") "Username to login with."
       , Option ['p'] ["password"] (ReqArg Password "password") "Password to use."
       , Option ['?'] ["help"] (NoArg Help) "Help text." ]

toConn (Host s) = ("host", s)
toConn (Database s) = ("dbname", s)
toConn (User s) = ("user", s)
toConn (Password s) = ("password", s)
toConn Help = ("", "")

main = do
  (args, _, err) <- getArgs >>= return . getOpt RequireOrder opts 
  when (not . null $ err) $ do { mapM_ putStrLn err;
                                 exitWith $ ExitFailure 1 }
  when (null args || Help `elem` args) $ do { putStrLn (usageInfo "" opts);
                                 exitWith ExitSuccess }
  dbTestMain $ Conn { dbLabel = "hdbc-postgresql"
                    , dbConn = connect driver (map toConn args) }
