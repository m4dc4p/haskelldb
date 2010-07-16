-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HDBC
-- Copyright   :  HWT Group 2003, 
--                Bjorn Bringert 2005-2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  portable
--
-- HDBC interface for HaskellDB
--
-----------------------------------------------------------

module Database.HaskellDB.HDBC (hdbcConnect) where

import Database.HaskellDB
import Database.HaskellDB.Database
import Database.HaskellDB.Sql.Generate (SqlGenerator(..))
import Database.HaskellDB.Sql.Print
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query
import Database.HaskellDB.FieldType

import Database.HDBC as HDBC hiding (toSql)

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- | Run an action on a HDBC IConnection and close the connection.
hdbcConnect :: (MonadIO m, IConnection conn) => 
               SqlGenerator
            -> IO conn -- ^ connection function
	    -> (Database -> m a) -> m a
hdbcConnect gen connect action = 
    do
    conn <- liftIO $ handleSqlError connect
    x <- action (mkDatabase gen conn)
    -- FIXME: should we really commit here?
    liftIO $ HDBC.commit conn
    liftIO $ handleSqlError (HDBC.disconnect conn)
    return x

mkDatabase :: (IConnection conn) => SqlGenerator -> conn -> Database
mkDatabase gen connection
    = Database { dbQuery	= hdbcQuery       gen connection,
    		 dbInsert	= hdbcInsert      gen connection,
		 dbInsertQuery 	= hdbcInsertQuery gen connection,
		 dbDelete	= hdbcDelete      gen connection,
		 dbUpdate	= hdbcUpdate      gen connection,
		 dbTables       = hdbcTables          connection,
		 dbDescribe     = hdbcDescribe        connection,
		 dbTransaction  = hdbcTransaction     connection,
		 dbCreateDB     = hdbcCreateDB    gen connection,
		 dbCreateTable  = hdbcCreateTable gen connection,
		 dbDropDB       = hdbcDropDB      gen connection,
		 dbDropTable    = hdbcDropTable   gen connection
	       }

hdbcQuery :: (GetRec er vr, IConnection conn) => 
	     SqlGenerator
          -> conn
	  -> PrimQuery 
	  -> Rel er
	  -> IO [Record vr]
hdbcQuery gen connection q rel = hdbcPrimQuery connection sql scheme rel
    where sql = show $ ppSql $ sqlQuery gen q
          scheme = attributes q

hdbcInsert :: (IConnection conn) => SqlGenerator -> conn -> TableName -> Assoc -> IO ()
hdbcInsert gen conn table assoc = 
    hdbcPrimExecute conn $ show $ ppInsert $ sqlInsert gen table assoc

hdbcInsertQuery :: (IConnection conn) => SqlGenerator -> conn -> TableName -> PrimQuery -> IO ()
hdbcInsertQuery gen conn table assoc = 
    hdbcPrimExecute conn $ show $ ppInsert $ sqlInsertQuery gen table assoc

hdbcDelete :: (IConnection conn) => SqlGenerator -> conn -> TableName -> [PrimExpr] -> IO ()
hdbcDelete gen conn table exprs = 
    hdbcPrimExecute conn $ show $ ppDelete $ sqlDelete gen table exprs

hdbcUpdate :: (IConnection conn) => SqlGenerator -> conn -> TableName -> [PrimExpr] -> Assoc -> IO ()
hdbcUpdate gen conn table criteria assigns = 
    hdbcPrimExecute conn $ show $ ppUpdate $ sqlUpdate gen table criteria assigns

hdbcTables :: (IConnection conn) => conn -> IO [TableName]
hdbcTables conn = handleSqlError $ HDBC.getTables conn

hdbcDescribe :: (IConnection conn) => conn -> TableName -> IO [(Attribute,FieldDesc)]
hdbcDescribe conn table = 
    handleSqlError $ do
                     cs <- HDBC.describeTable conn table
                     return [(n,colDescToFieldDesc c) | (n,c) <- cs]

colDescToFieldDesc :: SqlColDesc -> FieldDesc
colDescToFieldDesc c = (t, nullable)
    where 
    nullable = fromMaybe True (colNullable c)
    string = maybe StringT BStrT (colSize c)
    t = case colType c of
            SqlCharT          -> string
            SqlVarCharT       -> string
            SqlLongVarCharT   -> string
            SqlWCharT	      -> string
            SqlWVarCharT      -> string
            SqlWLongVarCharT  -> string
            SqlDecimalT       -> IntegerT
            SqlNumericT       -> IntegerT
            SqlSmallIntT      -> IntT
            SqlIntegerT	      -> IntT
            SqlRealT	      -> DoubleT
            SqlFloatT	      -> DoubleT
            SqlDoubleT	      -> DoubleT
            SqlBitT	      -> BoolT
            SqlTinyIntT	      -> IntT
            SqlBigIntT	      -> IntT
            SqlBinaryT	      -> string
            SqlVarBinaryT     -> string
            SqlLongVarBinaryT -> string
            SqlDateT          -> CalendarTimeT
            SqlTimeT          -> CalendarTimeT
            SqlTimestampT     -> CalendarTimeT
            SqlUTCDateTimeT   -> CalendarTimeT
            SqlUTCTimeT       -> CalendarTimeT
            SqlTimeWithZoneT  -> CalendarTimeT
            SqlTimestampWithZoneT -> CalendarTimeT
            SqlIntervalT _    -> string
            SqlGUIDT          -> string
            SqlUnknownT _     -> string

hdbcCreateDB :: (IConnection conn) => SqlGenerator -> conn -> String -> IO ()
hdbcCreateDB gen conn name 
    = hdbcPrimExecute conn $ show $ ppCreate $ sqlCreateDB gen name

hdbcCreateTable :: (IConnection conn) => SqlGenerator -> conn -> TableName -> [(Attribute,FieldDesc)] -> IO ()
hdbcCreateTable gen conn name attrs
    = hdbcPrimExecute conn $ show $ ppCreate $ sqlCreateTable gen name attrs

hdbcDropDB :: (IConnection conn) => SqlGenerator -> conn -> String -> IO ()
hdbcDropDB gen conn name 
    = hdbcPrimExecute conn $ show $ ppDrop $ sqlDropDB gen name

hdbcDropTable :: (IConnection conn) => SqlGenerator -> conn -> TableName -> IO ()
hdbcDropTable gen conn name
    = hdbcPrimExecute conn $ show $ ppDrop $ sqlDropTable gen name

-- | HDBC implementation of 'Database.dbTransaction'.
hdbcTransaction :: (IConnection conn) => conn -> IO a -> IO a
hdbcTransaction conn action = 
    handleSqlError $ HDBC.withTransaction conn (\_ -> action)


-----------------------------------------------------------
-- Primitive operations
-----------------------------------------------------------

type HDBCRow = Map String HDBC.SqlValue

-- | Primitive query
hdbcPrimQuery :: (GetRec er vr, IConnection conn) => 
		 conn -- ^ Database connection.
	      -> String     -- ^ SQL query
	      -> Scheme     -- ^ List of field names to retrieve
	      -> Rel er   -- ^ Phantom argument to get the return type right.
	      -> IO [Record vr]    -- ^ Query results
hdbcPrimQuery conn sql scheme rel = 
    do
    stmt <- handleSqlError $ HDBC.prepare conn sql
    handleSqlError $ HDBC.execute stmt []
    rows <- HDBC.fetchAllRowsMap stmt
    mapM (getRec hdbcGetInstances rel scheme) rows

-- | Primitive execute
hdbcPrimExecute :: (IConnection conn) => conn -- ^ Database connection.
		-> String     -- ^ SQL query.
		-> IO ()
hdbcPrimExecute conn sql = 
    do
    handleSqlError $ HDBC.run conn sql []
    return ()


-----------------------------------------------------------
-- Getting data from a statement
-----------------------------------------------------------

hdbcGetInstances :: GetInstances HDBCRow
hdbcGetInstances = 
    GetInstances {
		  getString        = hdbcGetValue
		 , getInt          = hdbcGetValue
		 , getInteger      = hdbcGetValue
		 , getDouble       = hdbcGetValue
		 , getBool         = hdbcGetValue
		 , getCalendarTime = hdbcGetValue
		 }

-- hdbcGetValue :: Data.Convertible.Base.Convertible SqlValue a
--             => HDBCRow -> String -> IO (Maybe a)
hdbcGetValue m f = case Map.lookup (map toLower f) m of
                     Nothing -> fail $ "No such field " ++ f
                     Just x  -> return $ HDBC.fromSql x
