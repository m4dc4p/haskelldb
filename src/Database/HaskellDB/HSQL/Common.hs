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
--		   , HSQL
		   ) where

import Data.Dynamic
import Data.Maybe
import Control.Monad
import System.Time

import Database.HaskellDB.HDBRec
import Database.HaskellDB.Database
import Database.HaskellDB.Sql
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query
import Database.HaskellDB.FieldType
import Database.HaskellDB.BoundedString
import Database.HaskellDB.BoundedList

import Database.HSQL as HSQL

-- | Run an action on a HSQL Connection and close the connection.
hsqlConnect :: (opts -> IO Connection) -- ^ HSQL connection function, e.g.  
	    -> opts -> (Database -> IO a) -> IO a
hsqlConnect connect opts action = 
    do
    conn <- handleSqlError (connect opts)
    x <- handleSqlError (action (newHSQL conn))
    disconnect conn
    return x

handleSqlError :: IO a -> IO a
handleSqlError io = handleSql (\err -> fail (show err)) io

newHSQL :: Connection -> Database
newHSQL connection
    = Database { dbQuery	= hsqlQuery' connection,
    		 dbInsert	= hsqlInsert connection,
		 dbInsertQuery 	= hsqlInsertQuery connection,
		 dbDelete	= hsqlDelete connection,
		 dbUpdate	= hsqlUpdate connection,
		 dbTables       = hsqlTables connection,
		 dbDescribe     = hsqlDescribe connection,
		 dbTransaction  = hsqlTransaction connection
	       }

hsqlInsert conn table assoc = 
    hsqlPrimExecute conn $ show $ ppInsert $ toInsert table assoc
	  
hsqlInsertQuery conn table assoc = 
    hsqlPrimExecute conn $ show $ ppInsert $ toInsertQuery table assoc
	  
hsqlDelete conn table exprs = 
    hsqlPrimExecute conn $ show $ ppDelete $ toDelete table exprs

hsqlUpdate conn table criteria assigns = 
    hsqlPrimExecute conn $ show $ ppUpdate $ toUpdate table criteria assigns

hsqlTables :: Connection -> IO [TableName]
hsqlTables = HSQL.tables

hsqlDescribe :: Connection -> TableName -> IO [(Attribute,FieldDesc)]
hsqlDescribe conn table = liftM (map toFieldDesc) (HSQL.describe conn table)
   where
   toFieldDesc (name,sqlType,nullable) = (name,(toFieldType sqlType, nullable))

-- | HSQL implementation of 'Database.dbTransaction'.
hsqlTransaction :: Connection -> IO a -> IO a
hsqlTransaction conn action = inTransaction conn (\_ -> action)

toFieldType :: SqlType -> FieldType
toFieldType (SqlDecimal _ _) = DoubleT
toFieldType (SqlNumeric _ _) = DoubleT
toFieldType SqlSmallInt      = IntT
toFieldType SqlInteger       = IntT
toFieldType SqlReal          = DoubleT
toFieldType SqlFloat         = DoubleT
toFieldType SqlDouble        = DoubleT
--toFieldType SqlBit           = ?
toFieldType SqlTinyInt       = IntT
toFieldType SqlBigInt        = IntegerT
toFieldType SqlDate          = CalendarTimeT
toFieldType SqlTime          = CalendarTimeT
toFieldType SqlTimeStamp     = CalendarTimeT
toFieldType SqlDateTime      = CalendarTimeT
toFieldType (SqlVarChar a)   = BStrT a
toFieldType _                = StringT


-----------------------------------------------------------
-- Primitive Query
-- The "Rel r" argument is a phantom argument to get
-- the return type right.
-----------------------------------------------------------

mkIOMBCalendarTime :: Maybe ClockTime -> IO (Maybe CalendarTime)
mkIOMBCalendarTime Nothing = return Nothing
mkIOMBCalendarTime (Just c) = return (Just (mkCalendarTime c))


hsqlPrimExecute :: Connection -> String -> IO ()
hsqlPrimExecute connection sql = 
    do
    -- FIXME: (DEBUG) remove
    --putStrLn sql
    execute connection sql



--
-- New way of getting data, does not use Dynamic.
--


hsqlQuery' :: GetRec er vr => Connection -> PrimQuery -> Rel er -> IO [vr]
hsqlQuery' connection qtree rel
    = do
      rows <- hsqlPrimQuery' connection sql scheme rel
      return rows
    where
      sql = show (ppSql (toSql qtree))  
      scheme = attributes qtree

hsqlPrimQuery' :: GetRec er vr => Connection -> String -> Scheme -> Rel er -> IO [vr]
hsqlPrimQuery' connection sql scheme rel = 
    do
    stmt <- HSQL.query connection sql
    collectRows (getRec hsqlValueFuncs rel scheme) stmt


hsqlValueFuncs :: ValueFuncs Statement
hsqlValueFuncs = ValueFuncs {
			     getString        = getFieldValueMB
			    , getInt          = getFieldValueMB
			    , getInteger      = getFieldValueMB
			    , getDouble       = getFieldValueMB
			    , getCalendarTime = hsqlGetCalendarTime
			    }

hsqlGetCalendarTime :: Statement -> String -> IO (Maybe CalendarTime)
hsqlGetCalendarTime s f = getFieldValueMB s f >>= mkIOMBCalendarTime


