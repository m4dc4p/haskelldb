-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HSQL
-- Copyright   :  HWT Group 2003,
--                Bjorn Bringert 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  portable
--
-- HSQL interface for HaskellDB. You will also
-- need one of the back-end specific modules.
--
-----------------------------------------------------------

module Database.HaskellDB.HSQL (hsqlConnect) where

import Data.Maybe
import Control.Exception (catch, throwIO)
import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Time

import Database.HaskellDB
import Database.HaskellDB.Database
import Database.HaskellDB.Sql.Generate (SqlGenerator(..))
import Database.HaskellDB.Sql.Print
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query
import Database.HaskellDB.FieldType

import Database.HSQL as HSQL

-- | Run an action on a HSQL Connection and close the connection.
hsqlConnect :: MonadIO m => 
               SqlGenerator
            -> IO Connection -- ^ HSQL connection function
	    -> (Database -> m a) -> m a
hsqlConnect gen connect action = 
    do
    conn <- liftIO $ handleSqlError connect
    x <- action (mkDatabase gen conn)
    liftIO $ handleSqlError (disconnect conn)
    return x

handleSqlError :: IO a -> IO a
handleSqlError io = handleSql (\err -> fail (show err)) io

mkDatabase :: SqlGenerator -> Connection -> Database
mkDatabase gen connection
    = Database { dbQuery	= hsqlQuery       gen connection,
    		 dbInsert	= hsqlInsert      gen connection,
		 dbInsertQuery 	= hsqlInsertQuery gen connection,
		 dbDelete	= hsqlDelete      gen connection,
		 dbUpdate	= hsqlUpdate      gen connection,
		 dbTables       = hsqlTables          connection,
		 dbDescribe     = hsqlDescribe        connection,
		 dbTransaction  = hsqlTransaction     connection,
		 dbCreateDB     = hsqlCreateDB    gen connection,
		 dbCreateTable  = hsqlCreateTable gen connection,
		 dbDropDB       = hsqlDropDB      gen connection,
		 dbDropTable    = hsqlDropTable   gen connection
	       }

hsqlQuery :: GetRec er vr => 
	     SqlGenerator
          -> Connection 
	  -> PrimQuery 
	  -> Rel er 
	  -> IO [Record vr]
hsqlQuery gen connection q rel = hsqlPrimQuery connection sql scheme rel
    where
      sql = show $ ppSql $ sqlQuery gen q
      scheme = attributes q

hsqlInsert :: SqlGenerator -> Connection -> TableName -> Assoc -> IO ()
hsqlInsert gen conn table assoc = 
    hsqlPrimExecute conn $ show $ ppInsert $ sqlInsert gen table assoc

hsqlInsertQuery :: SqlGenerator -> Connection -> TableName -> PrimQuery -> IO ()
hsqlInsertQuery gen conn table assoc = 
    hsqlPrimExecute conn $ show $ ppInsert $ sqlInsertQuery gen table assoc

hsqlDelete :: SqlGenerator -> Connection -> TableName -> [PrimExpr] -> IO ()
hsqlDelete gen conn table exprs = 
    hsqlPrimExecute conn $ show $ ppDelete $ sqlDelete gen table exprs

hsqlUpdate :: SqlGenerator -> Connection -> TableName -> [PrimExpr] -> Assoc -> IO ()
hsqlUpdate gen conn table criteria assigns = 
    hsqlPrimExecute conn $ show $ ppUpdate $ sqlUpdate gen table criteria assigns

hsqlTables :: Connection -> IO [TableName]
hsqlTables conn = handleSqlError $ HSQL.tables conn

hsqlDescribe :: Connection -> TableName -> IO [(Attribute,FieldDesc)]
hsqlDescribe conn table = 
    handleSqlError $ liftM (map toFieldDesc) (HSQL.describe conn table)
   where
   toFieldDesc (name,sqlType,nullable) = (name,(toFieldType sqlType, nullable))

hsqlCreateDB :: SqlGenerator -> Connection -> String -> IO ()
hsqlCreateDB gen conn name 
    = hsqlPrimExecute conn $ show $ ppCreate $ sqlCreateDB gen name

hsqlCreateTable :: SqlGenerator -> Connection -> TableName -> [(Attribute,FieldDesc)] -> IO ()
hsqlCreateTable gen conn name as
    = hsqlPrimExecute conn $ show $ ppCreate $ sqlCreateTable gen name as

hsqlDropDB :: SqlGenerator -> Connection -> String -> IO ()
hsqlDropDB gen conn name 
    = hsqlPrimExecute conn $ show $ ppDrop $ sqlDropDB gen name

hsqlDropTable :: SqlGenerator -> Connection -> TableName -> IO ()
hsqlDropTable gen conn name
    = hsqlPrimExecute conn $ show $ ppDrop $ sqlDropTable gen name


toFieldType :: SqlType -> FieldType
toFieldType (SqlDecimal _ _) = DoubleT
toFieldType (SqlNumeric _ _) = DoubleT
toFieldType SqlSmallInt      = IntT
toFieldType SqlInteger       = IntT
toFieldType SqlReal          = DoubleT
toFieldType SqlFloat         = DoubleT
toFieldType SqlDouble        = DoubleT
-- toFieldType SqlBit           = BoolT
toFieldType SqlTinyInt       = IntT
toFieldType SqlMedInt        = IntT
toFieldType SqlBigInt        = IntegerT
toFieldType SqlDate          = CalendarTimeT
toFieldType SqlTime          = CalendarTimeT
toFieldType SqlTimeStamp     = CalendarTimeT
toFieldType SqlDateTime      = CalendarTimeT
toFieldType (SqlChar n)      = BStrT n
toFieldType (SqlVarChar n)   = BStrT n
toFieldType (SqlBinary n)    = BStrT n
toFieldType (SqlVarBinary n) = BStrT n
toFieldType _                = StringT

-- | HSQL implementation of 'Database.dbTransaction'.
hsqlTransaction :: Connection -> IO a -> IO a
hsqlTransaction conn action = 
    handleSqlError $ inTransaction conn (\_ -> action)


-----------------------------------------------------------
-- Primitive operations
-----------------------------------------------------------

-- | Primitive query
hsqlPrimQuery :: GetRec er vr => 
		 Connection -- ^ Database connection.
	      -> String     -- ^ SQL query
	      -> Scheme     -- ^ List of field names to retrieve
	      -> Rel er     -- ^ Phantom argument to get the return type right.
	      -> IO [Record vr]    -- ^ Query results
hsqlPrimQuery connection sql scheme rel = 
    do trace "HSQL.query" sql
       stmt <- handleSqlError $ HSQL.query connection sql
       getRows (getRec hsqlGetInstances rel scheme) stmt

-- | Retrive rows strictly.
getRows :: (Statement -> IO a) -> Statement -> IO [a]
getRows f stmt = handleSqlError loop
    where
    loop = do
	   success <- fetch stmt `onError` closeStatement stmt
	   if success 
	      then do
		   x <- f stmt `onError` closeStatement stmt
		   xs <- getRows f stmt
		   return (x:xs)
	      else do
		   closeStatement stmt
		   return []

onError :: IO a -> IO b -> IO a
onError a h = a `Control.Exception.catch` (\e -> h >> throwIO e)

-- | Primitive execute
hsqlPrimExecute :: Connection -- ^ Database connection.
		-> String     -- ^ SQL query.
		-> IO ()
hsqlPrimExecute connection sql = 
    do trace "HSQL.execute" sql
       handleSqlError (execute connection sql >> return ())


-----------------------------------------------------------
-- Getting data from a statement
-----------------------------------------------------------

hsqlGetInstances :: GetInstances Statement
hsqlGetInstances = 
    GetInstances {
		  getString        = getFieldValue
		 , getInt          = getFieldValue
		 , getInteger      = getFieldValue
		 , getDouble       = getFieldValue
		 , getBool         = getFieldValue
		 , getCalendarTime = hsqlGetCalendarTime
		 }

hsqlGetCalendarTime :: Statement -> String -> IO (Maybe CalendarTime)
hsqlGetCalendarTime s f = getFieldValue s f >>= mkIOMBCalendarTime

mkIOMBCalendarTime :: Maybe ClockTime -> IO (Maybe CalendarTime)
mkIOMBCalendarTime = maybe (return Nothing) (fmap Just . toCalendarTime)

-----------------------------------------------------------
-- Tracing
-----------------------------------------------------------

tracingEnabled :: IO Bool
tracingEnabled = return False

traceFile :: IO (Maybe FilePath)
traceFile = return Nothing

trace :: String -> String -> IO ()
trace act sql = 
    do t <- tracingEnabled
       when t $ do let s = act ++ ": " ++ sql
                   mf <- traceFile
                   case mf of
                     Nothing -> hPutStrLn stderr s
                     Just f  -> appendFile f s
