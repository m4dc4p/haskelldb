{-
 HSQL interface for HaskellDB
-}

module HSQL_driver where

import Data.Dynamic
import Monad
import Time

import Database hiding (query)
import Sql
import PrimQuery
import Query

import Database.ODBC.HSQL 

type ODBC = Database Connection (ODBCRow)

data ODBCRow r = ODBCRow [(Attribute,ODBCValue)]

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
		 database	= connection
	       }


odbcRowSelect :: Typeable a => Attr r a -> ODBCRow r -> a
odbcRowSelect attr (ODBCRow vals)
        = case lookup (attributeName attr) vals of
            Nothing  -> error "Query.rowSelect: invalid attribute used ??"
            Just dyn -> case fromDynamic dyn of
	                  Nothing -> error "Query.rowSelect: type mismatch"
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
    = odbcPrimQuery connection sql scheme rel
    where
      sql = show (ppSql (toSql qtree))  
      scheme = attributes qtree

-----------------------------------------------------------
-- Primitive Query
-- The "Rel r" argument is a phantom argument to get
-- the return type right.
-----------------------------------------------------------

odbcPrimQuery :: Connection -> String -> Scheme -> Rel r -> IO [ODBCRow r]
odbcPrimQuery connection sql scheme r = 
    do
    -- DEBUG:
    putStrLn sql
    stmt <- query connection sql
    -- FIXME: not lazy?
    collectRows (getRow scheme) stmt

getRow :: Scheme -> Statement -> IO (ODBCRow r)
getRow scheme stmt = 
    do
    vals <- mapM (getField stmt) scheme
    return (ODBCRow (zip scheme vals))

getField :: Statement -> Attribute -> IO ODBCValue
getField s n = 
    case t of
	   SqlChar _ -> liftM toDyn (getFieldValue s n::IO String)
	   SqlVarChar _ -> liftM toDyn (getFieldValue s n::IO String)
	   SqlLongVarChar _ -> liftM toDyn (getFieldValue s n::IO String)
	   SqlDecimal _ _ -> liftM toDyn (getFieldValue s n::IO Double)
	   SqlNumeric _ _ -> liftM toDyn (getFieldValue s n::IO Double)
	   SqlSmallInt -> liftM toDyn (getFieldValue s n::IO Int)
	   SqlInteger -> liftM toDyn (getFieldValue s n::IO Int)
	   SqlReal -> liftM toDyn (getFieldValue s n::IO Double)
	   SqlDouble -> liftM toDyn (getFieldValue s n::IO Double)
	   --SqlBit -> liftM toDyn (getFieldValue s n::IO Bool?)
	   --SqlTinyInt -> liftM toDyn (getFieldValue s n::IO Int?)
	   SqlBigInt -> liftM toDyn (getFieldValue s n::IO Int)
	   --SqlBinary _ -> liftM toDyn (getFieldValue s n::IO ?)
	   --SqlVarBinary _ -> liftM toDyn (getFieldValue s n::IO ?)
	   --SqlLongVarBinary _ -> liftM toDyn (getFieldValue s n::IO ?)
	   -- FIXME: no instance Typeable ClockTime
	   --SqlDate -> liftM toDyn (getFieldValue s n::IO ClockTime)
	   --SqlTime -> liftM toDyn (getFieldValue s n::IO ?)
	   --SqlTimeStamp -> liftM toDyn (getFieldValue s n::IO ?)
	where (t,nullable) = getFieldValueType s n
