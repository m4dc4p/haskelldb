-----------------------------------------------------------
-- |
-- Module      :  HaskellDB
-- Copyright   :  HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- HSQL interface for HaskellDB
-----------------------------------------------------------

module Database.HaskellDB.HSQL.Common (
		     hsqlConnect
		   ) where

import Data.Maybe
import Control.Exception (catch, throwIO)
import Control.Monad
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Time


import Database.HaskellDB.Database
import Database.HaskellDB.Sql
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query
import Database.HaskellDB.FieldType

import Database.HSQL as HSQL

-- | Run an action on a HSQL Connection and close the connection.
hsqlConnect :: (opts -> IO Connection) -- ^ HSQL connection function, e.g.  
	    -> opts -> (Database -> IO a) -> IO a
hsqlConnect connect opts action = 
    do
    conn <- handleSqlError (connect opts)
    x <- action (mkDatabase conn)
    handleSqlError (disconnect conn)
    return x

handleSqlError :: IO a -> IO a
handleSqlError io = handleSql (\err -> fail (show err)) io

mkDatabase :: Connection -> Database
mkDatabase connection
    = Database { dbQuery	= hsqlQuery connection,
    		 dbInsert	= hsqlInsert connection,
		 dbInsertQuery 	= hsqlInsertQuery connection,
		 dbDelete	= hsqlDelete connection,
		 dbUpdate	= hsqlUpdate connection,
		 dbTables       = hsqlTables connection,
		 dbDescribe     = hsqlDescribe connection,
		 dbTransaction  = hsqlTransaction connection,
		 dbCreateDB     = hsqlCreateDB connection,
		 dbCreateTable  = hsqlCreateTable connection,
		 dbDropDB       = hsqlDropDB connection,
		 dbDropTable    = hsqlDropTable connection
	       }

hsqlQuery :: GetRec er vr => 
	     Connection 
	  -> PrimQuery 
	  -> Rel er 
	  -> IO [vr]
hsqlQuery connection qtree rel = hsqlPrimQuery connection sql scheme rel
    where
      sql = show (ppSql (toSql qtree))  
      scheme = attributes qtree

hsqlInsert conn table assoc = 
    hsqlPrimExecute conn $ show $ ppInsert $ toInsert table assoc
	  
hsqlInsertQuery conn table assoc = 
    hsqlPrimExecute conn $ show $ ppInsert $ toInsertQuery table assoc
	  
hsqlDelete conn table exprs = 
    hsqlPrimExecute conn $ show $ ppDelete $ toDelete table exprs

hsqlUpdate conn table criteria assigns = 
    hsqlPrimExecute conn $ show $ ppUpdate $ toUpdate table criteria assigns

hsqlTables :: Connection -> IO [TableName]
hsqlTables conn = handleSqlError $ HSQL.tables conn

hsqlDescribe :: Connection -> TableName -> IO [(Attribute,FieldDesc)]
hsqlDescribe conn table = 
    handleSqlError $ liftM (map toFieldDesc) (HSQL.describe conn table)
   where
   toFieldDesc (name,sqlType,nullable) = (name,(toFieldType sqlType, nullable))

hsqlCreateDB :: Connection -> String -> IO ()
hsqlCreateDB conn name 
    = hsqlPrimExecute conn $ show $ ppCreate $ toCreateDB name
hsqlCreateTable :: Connection -> TableName -> [(Attribute,FieldDesc)] -> IO ()
hsqlCreateTable conn name as
    = hsqlPrimExecute conn $ show $ ppCreate $ toCreateTable name as
hsqlDropDB :: Connection -> String -> IO ()
hsqlDropDB conn name 
    = hsqlPrimExecute conn $ show $ ppDrop $ toDropDB name
hsqlDropTable :: Connection -> TableName -> IO ()
hsqlDropTable conn name
    = hsqlPrimExecute conn $ show $ ppDrop $ toDropTable name


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
	      -> IO [vr]    -- ^ Query results
hsqlPrimQuery connection sql scheme rel = 
    do
    stmt <- handleSqlError $ HSQL.query connection sql
    lazyRows (getRec hsqlGetInstances rel scheme) stmt

-- | Retrive rows lazily.
lazyRows :: (Statement -> IO a) -> Statement -> IO [a]
lazyRows f stmt = unsafeInterleaveIO (handleSqlError loop)
    where
    loop = do
	   success <- fetch stmt `onError` closeStatement stmt
	   if success 
	      then do
		   x <- f stmt `onError` closeStatement stmt
		   xs <- lazyRows f stmt
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
hsqlPrimExecute connection sql = handleSqlError $ execute connection sql


-----------------------------------------------------------
-- Getting data from a statement
-----------------------------------------------------------

hsqlGetInstances :: GetInstances Statement
hsqlGetInstances = 
    GetInstances {
		  getString        = getFieldValueMB
		 , getInt          = getFieldValueMB
		 , getInteger      = getFieldValueMB
		 , getDouble       = getFieldValueMB
		 , getBool         = getFieldValueMB
		 , getCalendarTime = hsqlGetCalendarTime
		 }

hsqlGetCalendarTime :: Statement -> String -> IO (Maybe CalendarTime)
hsqlGetCalendarTime s f = getFieldValueMB s f >>= mkIOMBCalendarTime

mkIOMBCalendarTime :: Maybe ClockTime -> IO (Maybe CalendarTime)
mkIOMBCalendarTime Nothing = return Nothing
mkIOMBCalendarTime (Just c) = return (Just (mkCalendarTime c))
