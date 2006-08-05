-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.WX
-- Copyright   :  HWT Group 2003, 
--                Bjorn Bringert 2005-2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- WxHaskell <http://wxhaskell.sourceforge.net/> 
-- interface for HaskellDB
--
-----------------------------------------------------------

module Database.HaskellDB.WX (
			      WXOptions(..), wxConnect,
                              DriverInterface(..), driver
			     ) where

import Data.Maybe
import Control.Exception (catch, throwIO)
import Control.Monad
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Time


import Database.HaskellDB
import Database.HaskellDB.DriverAPI
import Database.HaskellDB.Database
import Database.HaskellDB.Sql.Generate (SqlGenerator)
import Database.HaskellDB.Sql.Print
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query
import Database.HaskellDB.FieldType

import Graphics.UI.WXCore.WxcClasses (Db)
import Graphics.UI.WXCore.Db (DbRow, ColumnInfo(..),TableInfo(..),SqlType(..))
import qualified Graphics.UI.WXCore.Db as WX

type Connection = Db ()

data WXOptions = WXOptions { 
                            dsn :: String, -- ^ name binding in ODBC
                            uid :: String, -- ^ user id
                            pwd :: String  -- ^ password
                  	   }

-- | Run an action and close the connection.
wxConnect :: SqlGenerator -> WXOptions -> (Database -> IO a) -> IO a
wxConnect gen WXOptions{dsn=d,uid=u,pwd=p} action = 
    handleDbError (WX.dbWithConnection d u p (action . mkDatabase gen))

wxConnectOpts :: [(String,String)] -> (Database -> IO a) -> IO a
wxConnectOpts opts f = 
    do
    [a,b,c] <- getOptions ["dsn","uid","pwd"] opts
    gen <- getGenerator opts
    wxConnect gen (PostgreSQLOptions {dsn = a, uid = b, pwd = c}) f

driver :: DriverInterface
driver = defaultdriver {connect = wxConnectOpts}


handleDbError :: IO a -> IO a
handleDbError io = WX.catchDbError io (fail . WX.dbErrorMsg)

mkDatabase :: SqlGenerator -> Connection -> Database
mkDatabase gen connection
    = Database { dbQuery	= wxQuery       gen connection,
    		 dbInsert	= wxInsert      gen connection,
		 dbInsertQuery 	= wxInsertQuery gen connection,
		 dbDelete	= wxDelete      gen connection,
		 dbUpdate	= wxUpdate      gen connection,
		 dbTables       = wxTables          connection,
		 dbDescribe     = wxDescribe        connection,
		 dbTransaction  = wxTransaction     connection,
		 dbCreateDB     = wxCreateDB    gen connection,
		 dbCreateTable  = wxCreateTable gen connection,
		 dbDropDB       = wxDropDB      gen connection,
		 dbDropTable    = wxDropTable   gen connection
	       }

wxQuery :: GetRec er vr => SqlGenerator -> Connection -> PrimQuery -> Rel er -> IO [Record vr]
wxQuery gen connection qtree rel = wxPrimQuery connection sql scheme rel
    where
      sql = show $ ppSql $ sqlQuery gen qtree
      scheme = attributes qtree

wxInsert gen conn table assoc = 
    wxPrimExecute conn $ show $ ppInsert $ sqlInsert gen table assoc
	  
wxInsertQuery gen conn table assoc = 
    wxPrimExecute conn $ show $ ppInsert $ sqlInsertQuery gen table assoc
	  
wxDelete gen conn table exprs = 
    wxPrimExecute conn $ show $ ppDelete $ sqlDelete gen table exprs

wxUpdate gen conn table criteria assigns = 
    wxPrimExecute conn $ show $ ppUpdate $ sqlUpdate gen table criteria assigns

wxTables :: Connection -> IO [TableName]
wxTables conn = 
    handleDbError $ liftM (map tableName . WX.dbTables) (WX.dbGetInfo conn)

wxDescribe :: Connection -> TableName -> IO [(Attribute,FieldDesc)]
wxDescribe conn table = 
    do
    i <- handleDbError $ WX.dbGetTableInfo conn table
    return $ map toFieldDesc $ tableColumns i
    where 
    toFieldDesc ColumnInfo {columnName = name, 
			    columnSize = size,
			    columnSqlType = sqlType, 
			    columnNullable = nullable}
	= (name, (toFieldType size sqlType, nullable))

wxCreateDB :: SqlGenerator -> Connection -> String -> IO ()
wxCreateDB gen conn name 
    = wxPrimExecute conn $ show $ ppCreate $ sqlCreateDB gen name

wxCreateTable :: SqlGenerator -> Connection -> TableName -> [(Attribute,FieldDesc)] -> IO ()
wxCreateTable gen conn name as
    = wxPrimExecute conn $ show $ ppCreate $ sqlCreateTable gen name as

wxDropDB :: SqlGenerator -> Connection -> String -> IO ()
wxDropDB gen conn name 
    = wxPrimExecute conn $ show $ ppDrop $ sqlDropDB gen name

wxDropTable :: SqlGenerator -> Connection -> TableName -> IO ()
wxDropTable gen conn name
    = wxPrimExecute conn $ show $ ppDrop $ sqlDropTable gen name

toFieldType :: Int -> SqlType -> FieldType
toFieldType _ SqlDecimal   = DoubleT
toFieldType _ SqlNumeric   = DoubleT
toFieldType _ SqlReal      = DoubleT
toFieldType _ SqlFloat     = DoubleT
toFieldType _ SqlDouble    = DoubleT
toFieldType _ SqlSmallInt  = IntT
toFieldType _ SqlInteger   = IntT
toFieldType _ SqlTinyInt   = IntT
toFieldType _ SqlBigInt    = IntegerT
toFieldType _ SqlDate      = CalendarTimeT
toFieldType _ SqlTime      = CalendarTimeT
toFieldType _ SqlTimeStamp = CalendarTimeT
-- toFieldType _ SqlBit       = BoolT
toFieldType n SqlChar      = BStrT n
toFieldType n SqlVarChar   = BStrT n
toFieldType n SqlBinary    = BStrT n
toFieldType n SqlVarBinary = BStrT n
toFieldType _ _            = StringT


-- | WxHaskell implementation of 'Database.dbTransaction'.
wxTransaction :: Connection -> IO a -> IO a
wxTransaction conn action = handleDbError $ WX.dbTransaction conn action


-----------------------------------------------------------
-- Primitive operations
-----------------------------------------------------------

-- | Primitive query
wxPrimQuery :: GetRec er vr => 
	       Connection -- ^ Database connection.
	    -> String     -- ^ SQL query
	    -> Scheme     -- ^ List of field names to retrieve
	    -> Rel er     -- ^ Phantom argument to get the return type right.
	    -> IO [Record vr]    -- ^ Query results
wxPrimQuery connection sql scheme rel = 
    handleDbError $ WX.dbQuery connection sql getResults
	where getResults = getRec wxGetInstances rel scheme

-- | Primitive execute
--   FIXME: WxHaskell docs says to always wrap dbExecute in dbTransaction
wxPrimExecute :: Connection -- ^ Database connection.
		-> String     -- ^ SQL query.
		-> IO ()
wxPrimExecute connection sql = handleDbError $ WX.dbExecute connection sql


-----------------------------------------------------------
-- Getting data from a statement
-----------------------------------------------------------

wxGetInstances :: GetInstances (DbRow a)
wxGetInstances = 
    GetInstances {
		   getString       = WX.dbRowGetStringMb
		 , getInt          = WX.dbRowGetIntMb
		 , getInteger      = WX.dbRowGetIntegerMb
		 , getDouble       = WX.dbRowGetDoubleMb
		 , getBool         = WX.dbRowGetBoolMb
		 , getCalendarTime = wxGetCalendarTime
		 }

wxGetCalendarTime :: DbRow a -> String -> IO (Maybe CalendarTime)
wxGetCalendarTime r f = WX.dbRowGetClockTimeMb r f >>= mkIOMBCalendarTime

mkIOMBCalendarTime :: Maybe ClockTime -> IO (Maybe CalendarTime)
mkIOMBCalendarTime Nothing = return Nothing
mkIOMBCalendarTime (Just c) = return (Just (toUTCTime c))
