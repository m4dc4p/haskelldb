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
		   , HSQL
		   ) where

import Data.Dynamic
import Data.Maybe
import Control.Monad
import System.Time

import Database.HaskellDB.Database
import Database.HaskellDB.Sql
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query
import Database.HaskellDB.FieldType
import Database.HaskellDB.BoundedString
import Database.HaskellDB.BoundedList

import Database.HSQL as HSQL

type HSQL = Database Connection HSQLRow

data HSQLRow r = HSQLRow [(Attribute,HSQLValue)] deriving Show

type HSQLValue = Dynamic

-- Enable selection in an HSQL row
instance Typeable a => Row HSQLRow a where
    rowSelect = hsqlRowSelect

instance Typeable a => Row HSQLRow (Maybe a) where
    rowSelect = hsqlRowSelectMB

instance  Size n => Row HSQLRow (BoundedString n) where
    rowSelect = hsqlRowSelectBStr

-- | Run an action on a HSQL Connection and close the connection.
hsqlConnect :: (opts -> IO Connection) -- ^ HSQL connection function, e.g.  
	    -> opts -> (HSQL -> IO a) -> IO a
hsqlConnect connect opts action = 
    do
    conn <- handleSqlError (connect opts)
    x <- handleSqlError (action (newHSQL conn))
    disconnect conn
    return x

handleSqlError :: IO a -> IO a
handleSqlError io = handleSql (\err -> fail (show err)) io

newHSQL :: Connection -> HSQL
newHSQL connection
    = Database { dbQuery	= hsqlQuery connection,
    		 dbInsert	= hsqlInsert connection,
		 dbInsertQuery 	= hsqlInsertQuery connection,
		 dbDelete	= hsqlDelete connection,
		 dbUpdate	= hsqlUpdate connection,
		 dbTables       = hsqlTables connection,
		 dbDescribe     = hsqlDescribe connection,
		 dbTransaction  = hsqlTransaction connection
	       }


hsqlRowSelect' :: (Typeable a, Typeable b) => Attr f r a -> HSQLRow r1 -> (Maybe b)
hsqlRowSelect' attr (HSQLRow vals)
        = case lookup (attributeName attr) vals of
            Nothing  -> error "Query.rowSelect: invalid attribute used ??"
            Just dyn -> case fromDynamic dyn of
	                  Nothing -> 
			      error ("Query.rowSelect: type mismatch: " 
				     ++ attributeName attr ++ " :: " ++ show dyn)
			  Just val -> val

hsqlRowSelectMB :: Typeable a => Attr f r (Maybe a) -> HSQLRow r -> (Maybe a)
hsqlRowSelectMB = hsqlRowSelect'

hsqlRowSelectBStr :: Size n => Attr f r (BoundedString n) 
		     -> HSQLRow r -> BoundedString n
hsqlRowSelectBStr attr vals = case (hsqlRowSelect' attr vals) of
			    Nothing -> error ("Query.rowSelect: Null returned from non-nullable field")
			    Just val -> trunc val

hsqlRowSelect :: Typeable a => Attr f r a -> HSQLRow r -> a
hsqlRowSelect attr vals = case (hsqlRowSelect' attr vals) of
			    Nothing -> error ("Query.rowSelect: Null returned from non-nullable field")
			    Just val -> val

hsqlInsert conn table assoc = 
    hsqlPrimExecute conn $ show $ ppInsert $ toInsert table assoc
	  
hsqlInsertQuery conn table assoc = 
    hsqlPrimExecute conn $ show $ ppInsert $ toInsertQuery table assoc
	  
hsqlDelete conn table exprs = 
    hsqlPrimExecute conn $ show $ ppDelete $ toDelete table exprs

hsqlUpdate conn table criteria assigns = 
    hsqlPrimExecute conn $ show $ ppUpdate $ toUpdate table criteria assigns

hsqlQuery :: Connection -> PrimQuery -> Rel r -> IO [HSQLRow r]
hsqlQuery connection qtree rel
    = do
      rows <- hsqlPrimQuery connection sql scheme rel
      return rows
    where
      sql = show (ppSql (toSql qtree))  
      scheme = attributes qtree

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

hsqlPrimQuery :: Connection -> String -> Scheme -> Rel r -> IO [HSQLRow r]
hsqlPrimQuery connection sql scheme _ = 
    do
    -- FIXME: (DEBUG) remove
    --putStrLn sql
    stmt <- HSQL.query connection sql
    -- FIXME: (DEBUG) remove
    -- putStrLn $ unlines $ map show $ getFieldsTypes stmt
    collectRows (getRow scheme) stmt

getRow :: Scheme -> Statement -> IO (HSQLRow r)
getRow scheme stmt = 
    do
    vals <- mapM (getField stmt) scheme
    return (HSQLRow (zip scheme vals))

getField :: Statement -> Attribute -> IO HSQLValue
getField s n = 
    case toFieldType t of
	    StringT  -> toVal (getFieldValueMB s n :: IO (Maybe String))
	    IntT     -> toVal (getFieldValueMB s n :: IO (Maybe Int))
	    IntegerT -> toVal (getFieldValueMB s n :: IO (Maybe Integer))
	    DoubleT  -> toVal (getFieldValueMB s n :: IO (Maybe Double))
	    CalendarTimeT -> toVal $ mkIOMBCalendarTime
			     (getFieldValueMB s n :: IO (Maybe ClockTime))
	    -- FIXME: should wo do it like this?
	    -- if so, must fix hsqlRowSelect to handle this
	    BStrT _ -> toVal (getFieldValueMB s n :: IO (Maybe String))
    where
    (t,_) = getFieldValueType s n
    toVal :: Typeable a => IO (Maybe a) -> IO HSQLValue
    toVal = liftM toDyn
    mkIOMBCalendarTime :: IO (Maybe ClockTime) -> IO (Maybe CalendarTime)
    mkIOMBCalendarTime a 
	= do
	  b <- a
	  case b of
		 Nothing -> return Nothing
		 Just c  -> return (Just (mkCalendarTime c))

hsqlPrimExecute :: Connection -> String -> IO ()
hsqlPrimExecute connection sql = 
    do
    -- FIXME: (DEBUG) remove
    --putStrLn sql
    execute connection sql
