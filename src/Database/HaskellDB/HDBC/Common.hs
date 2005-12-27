-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.HDBC.Common
-- Copyright   :  HWT Group (c) 2003, Bjorn Bringert (c) 2005
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  portable
--
-- HDBC interface for HaskellDB
--
-----------------------------------------------------------

module Database.HaskellDB.HDBC.Common (
		     hdbcConnect, MonadIO
		   ) where

import Database.HaskellDB
import Database.HaskellDB.Database
import Database.HaskellDB.Sql
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query
import Database.HaskellDB.FieldType

import Database.HDBC as HDBC hiding (toSql)

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map (Map)
import qualified Data.Map as Map

-- | Run an action on a HDBC Connection and close the connection.
hdbcConnect :: MonadIO m => (opts -> IO Connection) -- ^ connection function
	    -> opts -> (Database -> m a) -> m a
hdbcConnect connect opts action = 
    do
    conn <- liftIO $ handleSqlError (connect opts)
    x <- action (mkDatabase conn)
    -- FIXME: should we really commit here?
    liftIO $ HDBC.commit conn
    liftIO $ handleSqlError (HDBC.disconnect conn)
    return x

mkDatabase :: Connection -> Database
mkDatabase connection
    = Database { dbQuery	= hdbcQuery connection,
    		 dbInsert	= hdbcInsert connection,
		 dbInsertQuery 	= hdbcInsertQuery connection,
		 dbDelete	= hdbcDelete connection,
		 dbUpdate	= hdbcUpdate connection,
		 dbTables       = hdbcTables connection,
		 dbDescribe     = hdbcDescribe connection,
		 dbTransaction  = hdbcTransaction connection,
		 dbCreateDB     = hdbcCreateDB connection,
		 dbCreateTable  = hdbcCreateTable connection,
		 dbDropDB       = hdbcDropDB connection,
		 dbDropTable    = hdbcDropTable connection
	       }

hdbcQuery :: GetRec er vr => 
	     Connection 
	  -> PrimQuery 
	  -> Rel er 
	  -> IO [Record vr]
hdbcQuery connection qtree rel = hdbcPrimQuery connection sql scheme rel
    where
      sql = show (ppSql (toSql qtree))  
      scheme = attributes qtree

hdbcInsert :: Connection -> TableName -> Assoc -> IO ()
hdbcInsert conn table assoc = 
    hdbcPrimExecute conn $ show $ ppInsert $ toInsert table assoc

hdbcInsertQuery :: Connection -> TableName -> PrimQuery -> IO ()
hdbcInsertQuery conn table assoc = 
    hdbcPrimExecute conn $ show $ ppInsert $ toInsertQuery table assoc

hdbcDelete :: Connection -> TableName -> [PrimExpr] -> IO ()
hdbcDelete conn table exprs = 
    hdbcPrimExecute conn $ show $ ppDelete $ toDelete table exprs

hdbcUpdate :: Connection -> TableName -> [PrimExpr] -> Assoc -> IO ()
hdbcUpdate conn table criteria assigns = 
    hdbcPrimExecute conn $ show $ ppUpdate $ toUpdate table criteria assigns

-- FIXME: implement when HDBC supports it
hdbcTables :: Connection -> IO [TableName]
hdbcTables conn = fail "Listing tables not yet supported in HDBC"

hdbcDescribe :: Connection -> TableName -> IO [(Attribute,FieldDesc)]
hdbcDescribe conn table = 
    fail "Describing tables not yet supported in HDBC"

hdbcCreateDB :: Connection -> String -> IO ()
hdbcCreateDB conn name 
    = hdbcPrimExecute conn $ show $ ppCreate $ toCreateDB name

hdbcCreateTable :: Connection -> TableName -> [(Attribute,FieldDesc)] -> IO ()
hdbcCreateTable conn name attrs
    = hdbcPrimExecute conn $ show $ ppCreate $ toCreateTable name attrs

hdbcDropDB :: Connection -> String -> IO ()
hdbcDropDB conn name 
    = hdbcPrimExecute conn $ show $ ppDrop $ toDropDB name

hdbcDropTable :: Connection -> TableName -> IO ()
hdbcDropTable conn name
    = hdbcPrimExecute conn $ show $ ppDrop $ toDropTable name

-- | HDBC implementation of 'Database.dbTransaction'.
hdbcTransaction :: Connection -> IO a -> IO a
hdbcTransaction conn action = 
    handleSqlError $ HDBC.withTransaction conn (\_ -> action)


-----------------------------------------------------------
-- Primitive operations
-----------------------------------------------------------

type HDBCRow = Map String HDBC.SqlValue

-- | Primitive query
hdbcPrimQuery :: GetRec er vr => 
		 Connection -- ^ Database connection.
	      -> String     -- ^ SQL query
	      -> Scheme     -- ^ List of field names to retrieve
	      -> Rel er     -- ^ Phantom argument to get the return type right.
	      -> IO [Record vr]    -- ^ Query results
hdbcPrimQuery conn sql scheme rel = 
    do
    stmt <- handleSqlError $ HDBC.prepare conn sql
    handleSqlError $ HDBC.execute stmt []
    rows <- HDBC.fetchAllRowsMap stmt
    mapM (getRec hdbcGetInstances rel scheme) rows

-- | Primitive execute
hdbcPrimExecute :: Connection -- ^ Database connection.
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

hdbcGetValue :: SqlType a => HDBCRow -> String -> IO (Maybe a)
hdbcGetValue m f = return $ fmap HDBC.fromSql (Map.lookup f m)
