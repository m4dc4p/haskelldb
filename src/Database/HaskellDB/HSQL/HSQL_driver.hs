{-
 HSQL interface for HaskellDB

 TODO:
 - add Haddock comments
 - figure out date / time types
 - make odbcPrimQuery lazy
-}

module HSQL_driver (
		    ODBCOptions(..)
		   , odbcConnect
		   ) where

import Data.Dynamic
import Maybe
import Monad

import Database hiding (query)
import Sql
import PrimQuery
import Query
import FieldType

import Database.ODBC.HSQL as HSQL

type ODBC = Database Connection (ODBCRow)

data ODBCRow r = ODBCRow [(Attribute,ODBCValue)] deriving Show

type ODBCValue = Dynamic



-- Enable selection in an ODBC row
instance Typeable a => Row ODBCRow a where
    rowSelect = odbcRowSelect

data ODBCOptions = ODBCOptions { 
                               dsn :: String, --name binding in ODBC
                               uid :: String, --user id
                               pwd :: String  --password
                  	       }          
                             

odbcConnect :: ODBCOptions -> (ODBC -> IO a) -> IO a
odbcConnect opts action = do
			  conn <- connect (dsn opts) (uid opts) (pwd opts)
			  x <- action (newODBC conn)
			  disconnect conn
			  return x

newODBC :: Connection -> ODBC
newODBC connection
    = Database { dbQuery	= odbcQuery,
    		 dbInsert	= odbcInsert,
		 dbInsertNew 	= odbcInsertNew,
		 dbDelete	= odbcDelete,
		 dbUpdate	= odbcUpdate,
		 dbTables       = odbcTables,
		 dbDescribe     = odbcDescribe,
		 database	= connection
	       }


odbcRowSelect :: Typeable a => Attr f r a -> ODBCRow r -> a
odbcRowSelect attr (ODBCRow vals)
        = case lookup (attributeName attr) vals of
            Nothing  -> error "Query.rowSelect: invalid attribute used ??"
            Just dyn -> case fromDynamic dyn of
	                  Nothing -> 
			      error ("Query.rowSelect: type mismatch: " ++ show dyn)
			  Just val -> val

odbcInsertNew connection table assoc = execute connection sql
	where
	  sql = show (ppInsert (toInsertNew table assoc))
	  
odbcInsert connection table assoc = execute connection sql
	where
	  sql = show (ppInsert (toInsert table assoc))
	  
odbcDelete connection table exprs = execute connection sql    		
	where
	  sql = show (ppDelete (toDelete table exprs))
		  
odbcUpdate connection table criteria assigns = execute connection sql
	where
	  sql = show (ppUpdate (toUpdate table criteria assigns))	  

odbcQuery :: Connection -> PrimQuery -> Rel r -> IO [ODBCRow r]
odbcQuery connection qtree rel
    = do
      rows <- odbcPrimQuery connection sql scheme rel
      -- FIXME: remove
      --putStrLn (unlines (map show rows))
      return rows
    where
      sql = show (ppSql (toSql qtree))  
      scheme = attributes qtree

odbcTables :: Connection -> IO [TableName]
odbcTables = HSQL.tables

odbcDescribe :: Connection -> TableName -> IO [(Attribute,FieldDef)]
odbcDescribe conn table = liftM (map toFieldDef) (HSQL.describe conn table)
    where
    toFieldDef (name,sqlType,nullable) = (name,(toFieldType sqlType, nullable))

toFieldType :: SqlType -> FieldType
toFieldType (SqlDecimal _ _) = DoubleT
toFieldType (SqlNumeric _ _) = DoubleT
toFieldType SqlSmallInt      = IntT
toFieldType SqlInteger       = IntT
toFieldType SqlReal          = DoubleT
toFieldType SqlDouble        = DoubleT
--toFieldType SqlBit           = ?
toFieldType SqlTinyInt       = IntT
toFieldType SqlBigInt        = IntegerT
--toFieldType SqlDate          = ?
--toFieldType SqlTime          = ?
--toFieldType SqlTimeStamp     = ?
toFieldType _                = StringT


-----------------------------------------------------------
-- Primitive Query
-- The "Rel r" argument is a phantom argument to get
-- the return type right.
-----------------------------------------------------------

odbcPrimQuery :: Connection -> String -> Scheme -> Rel r -> IO [ODBCRow r]
odbcPrimQuery connection sql scheme _ = 
    do
    -- FIXME: (DEBUG) remove
    --putStrLn sql
    stmt <- query connection sql
    collectRows (getRow scheme) stmt

getRow :: Scheme -> Statement -> IO (ODBCRow r)
getRow scheme stmt = 
    do
    vals <- mapM (getField stmt) scheme
    return (ODBCRow (zip scheme vals))

getField :: Statement -> Attribute -> IO ODBCValue
getField s n = 
    case toFieldType t of
	    StringT  -> toVal (getFieldValueMB s n :: IO (Maybe String))
	    IntT     -> toVal (getFieldValueMB s n :: IO (Maybe Int))
	    IntegerT -> toVal (getFieldValueMB s n :: IO (Maybe Integer))
	    DoubleT  -> toVal (getFieldValueMB s n :: IO (Maybe Double))
    where
    (t,nullable) = getFieldValueType s n
    toVal :: Typeable a => IO (Maybe a) -> IO ODBCValue
    toVal m | nullable = liftM toDyn m
	    -- FIXME: what if we have Nothing?
	    | otherwise = liftM (toDyn . fromJust) m
