-----------------------------------------------------------
-- |
-- Module      :  HaskellDB
-- Copyright   :  HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  non-portable
--
-- WxHaskell <http://wxhaskell.sourceforge.net/> 
-- interface for HaskellDB
-----------------------------------------------------------

module Database.HaskellDB.WX (
			      wxConnect,
			      WXOptions(..)
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
wxConnect :: WXOptions -> (Database -> IO a) -> IO a
wxConnect WXOptions{dsn=d,uid=u,pwd=p} action = 
    handleDbError (WX.dbWithConnection d u p (action . mkDatabase))

handleDbError :: IO a -> IO a
handleDbError io = WX.catchDbError io (fail . WX.dbErrorMsg)

mkDatabase :: Connection -> Database
mkDatabase connection
    = Database { dbQuery	= wxQuery connection,
    		 dbInsert	= wxInsert connection,
		 dbInsertQuery 	= wxInsertQuery connection,
		 dbDelete	= wxDelete connection,
		 dbUpdate	= wxUpdate connection,
		 dbTables       = wxTables connection,
		 dbDescribe     = wxDescribe connection,
		 dbTransaction  = wxTransaction connection,
		 dbCreateDB     = wxCreateDB connection,
		 dbCreateTable  = wxCreateTable connection,
		 dbDropDB       = wxDropDB connection,
		 dbDropTable    = wxDropTable connection
	       }

wxQuery :: GetRec er vr => Connection -> PrimQuery -> Rel er -> IO [vr]
wxQuery connection qtree rel = wxPrimQuery connection sql scheme rel
    where
      sql = show (ppSql (toSql qtree))  
      scheme = attributes qtree

wxInsert conn table assoc = 
    wxPrimExecute conn $ show $ ppInsert $ toInsert table assoc
	  
wxInsertQuery conn table assoc = 
    wxPrimExecute conn $ show $ ppInsert $ toInsertQuery table assoc
	  
wxDelete conn table exprs = 
    wxPrimExecute conn $ show $ ppDelete $ toDelete table exprs

wxUpdate conn table criteria assigns = 
    wxPrimExecute conn $ show $ ppUpdate $ toUpdate table criteria assigns

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

wxCreateDB :: Connection -> String -> IO ()
wxCreateDB conn name 
    = wxPrimExecute conn $ show $ ppCreate $ toCreateDB name
wxCreateTable :: Connection -> TableName -> [(Attribute,FieldDesc)] -> IO ()
wxCreateTable conn name as
    = wxPrimExecute conn $ show $ ppCreate $ toCreateTable name as
wxDropDB :: Connection -> String -> IO ()
wxDropDB conn name 
    = wxPrimExecute conn $ show $ ppDrop $ toDropDB name
wxDropTable :: Connection -> TableName -> IO ()
wxDropTable conn name
    = wxPrimExecute conn $ show $ ppDrop $ toDropTable name

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
-- FIXME: make this lazy
wxPrimQuery :: GetRec er vr => 
	       Connection -- ^ Database connection.
	    -> String     -- ^ SQL query
	    -> Scheme     -- ^ List of field names to retrieve
	    -> Rel er     -- ^ Phantom argument to get the return type right.
	    -> IO [vr]    -- ^ Query results
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
mkIOMBCalendarTime (Just c) = return (Just (mkCalendarTime c))
