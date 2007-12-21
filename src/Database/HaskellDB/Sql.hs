-----------------------------------------------------------
-- |
-- Module      :  SQL
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- A data type for SQL.
--
-----------------------------------------------------------
module Database.HaskellDB.Sql ( 
                               SqlTable,
                               SqlColumn,
                               SqlOrder(..),
                               SqlType(..),

	                       SqlSelect(..), 
	                       SqlUpdate(..), 
	                       SqlDelete(..), 
	                       SqlInsert(..),
	                       SqlCreate(..), 
	                       SqlDrop(..),

                               SqlExpr(..),

                               newSelect
	                      ) where


-----------------------------------------------------------
-- * SQL data type
-----------------------------------------------------------

type SqlTable = String

type SqlColumn = String

data SqlOrder = SqlAsc | SqlDesc
  deriving Show

data SqlType = SqlType String
             | SqlType1 String Int
             | SqlType2 String Int Int
  deriving Show

-- | Data type for SQL SELECT statements.
data SqlSelect  = SqlSelect { 
                             options   :: [String],                -- ^ DISTINCT, ALL etc.
			     attrs     :: [(SqlColumn,SqlExpr)],   -- ^ result
                             tables    :: [(SqlTable,SqlSelect)],  -- ^ FROM
                             criteria  :: [SqlExpr],               -- ^ WHERE
                             groupby   :: [(SqlColumn,SqlExpr)],   -- ^ GROUP BY
                             orderby   :: [(SqlExpr,SqlOrder)],    -- ^ ORDER BY
			     extra     :: [String]                 -- ^ TOP n, etc.
                            }
                | SqlBin   String SqlSelect SqlSelect -- ^ Binary relational operator
                | SqlTable SqlTable -- ^ Select a whole table.
                | SqlEmpty -- ^ Empty select.
  deriving Show
  
-- | Expressions in SQL statements.
data SqlExpr = ColumnSqlExpr  SqlColumn
             | BinSqlExpr     String SqlExpr SqlExpr
             | PrefixSqlExpr  String SqlExpr
             | PostfixSqlExpr String SqlExpr
             | FunSqlExpr     String [SqlExpr]
             | ConstSqlExpr   String
	     | CaseSqlExpr    [(SqlExpr,SqlExpr)] SqlExpr
             | ListSqlExpr    [SqlExpr]
             | ExistsSqlExpr  SqlSelect
  deriving Show

-- | Data type for SQL UPDATE statements.
data SqlUpdate  = SqlUpdate SqlTable [(SqlColumn,SqlExpr)] [SqlExpr]

-- | Data type for SQL DELETE statements.
data SqlDelete  = SqlDelete SqlTable [SqlExpr]

-- | Data type for SQL INSERT statements.
data SqlInsert  = SqlInsert      SqlTable [SqlColumn] [SqlExpr]
                | SqlInsertQuery SqlTable [SqlColumn] SqlSelect

-- | Data type for SQL CREATE statements.
data SqlCreate = SqlCreateDB String -- ^ Create a database
	       | SqlCreateTable SqlTable [(SqlColumn,(SqlType,Bool))] -- ^ Create a table.

-- | Data type representing the SQL DROP statement.
data SqlDrop = SqlDropDB String -- ^ Delete a database
	     | SqlDropTable SqlTable -- ^ Delete a table named SqlTable

newSelect :: SqlSelect
newSelect = SqlSelect { 
                       options   = [],
                       attrs     = [],
                       tables    = [],
                       criteria  = [],
                       groupby	 = [],
                       orderby	 = [],
                       extra     = []
                      }

