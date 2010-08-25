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
                               SqlName,
                               SqlOrder(..),
                               SqlType(..),

	                       SqlSelect(..), 
	                       SqlUpdate(..), 
	                       SqlDelete(..), 
	                       SqlInsert(..),
	                       SqlCreate(..), 
	                       SqlDrop(..),

                               SqlExpr(..),
                               Mark(..),

                               newSelect, foldSqlExpr, foldSqlSelect
	                      ) where


-----------------------------------------------------------
-- * SQL data type
-----------------------------------------------------------

type SqlTable = String

type SqlColumn = String

-- | A valid SQL name for a parameter.
type SqlName = String

data SqlOrder = SqlAsc | SqlDesc
  deriving Show

data SqlType = SqlType String
             | SqlType1 String Int
             | SqlType2 String Int Int
  deriving Show

data Mark = All | Columns [(SqlColumn, SqlExpr)]
  deriving Show

-- | Data type for SQL SELECT statements.
data SqlSelect  = SqlSelect { 
                             options   :: [String],                -- ^ DISTINCT, ALL etc.
			     attrs     :: [(SqlColumn,SqlExpr)],   -- ^ result
                             tables    :: [(SqlTable,SqlSelect)],  -- ^ FROM
                             criteria  :: [SqlExpr],               -- ^ WHERE
                             groupby   :: Maybe Mark,   -- ^ GROUP BY
                             orderby   :: [(SqlExpr,SqlOrder)],    -- ^ ORDER BY
			     extra     :: [String]                 -- ^ TOP n, etc.
                            }
                | SqlBin   String SqlSelect SqlSelect -- ^ Binary relational operator
                | SqlTable SqlTable -- ^ Select a whole table.
                | SqlEmpty -- ^ Empty select.
  deriving Show

-- | Transform a SqlSelect value.
foldSqlSelect :: ([String] -> [(SqlColumn,SqlExpr)] 
                           -> [(SqlTable, t)] 
                           -> [SqlExpr] -> Maybe Mark 
                           -> [(SqlExpr,SqlOrder)] 
                           -> [String] -> t
                 , String -> t -> t -> t
                 , SqlTable -> t, t) 
              -> SqlSelect 
              -> t
foldSqlSelect (select, bin, table, empty) = fold
  where
    fold (SqlSelect opt attr tab crit grou ord ext) = select opt attr (map (\(t, s) -> (t, fold s)) tab) crit grou ord ext
    fold (SqlBin op left right) = bin op (fold left) (fold right)
    fold (SqlTable tab) = table tab
    fold SqlEmpty = empty

-- | Expressions in SQL statements.
data SqlExpr = ColumnSqlExpr  SqlColumn
             | BinSqlExpr     String SqlExpr SqlExpr
             | PrefixSqlExpr  String SqlExpr
             | PostfixSqlExpr String SqlExpr
             | FunSqlExpr     String [SqlExpr]
             | AggrFunSqlExpr String [SqlExpr] -- ^ Aggregate functions separate from normal functions.
             | ConstSqlExpr   String
	     | CaseSqlExpr    [(SqlExpr,SqlExpr)] SqlExpr
             | ListSqlExpr    [SqlExpr]
             | ExistsSqlExpr  SqlSelect
             | ParamSqlExpr (Maybe SqlName) SqlExpr
             | PlaceHolderSqlExpr
             | ParensSqlExpr SqlExpr
             | CastSqlExpr String SqlExpr 
  deriving Show

-- | Transform a SqlExpr value.
foldSqlExpr :: (SqlColumn -> t -- column
               , String -> t -> t -> t -- bin
               , String -> t -> t -- prefix
               , String -> t -> t -- postfix
               , String -> [t] -> t -- fun
               , String -> [t] -> t -- aggr
               , String -> t -- constant
	       , [(t,t)] -> t -> t -- _case
               , [t] -> t -- list
               , SqlSelect -> t -- exists
               , (Maybe SqlName) -> t -> t -- param
               , t -- placeHolder
               , t -> t -- parens
               , String -> t -> t {- casts -}) 
            -> SqlExpr 
            -> t
foldSqlExpr (column, bin, prefix, postfix, fun, aggr, constant, _case, list, exists, 
                   param, placeHolder, parens, casts) = fold
  where
    fold (ColumnSqlExpr col) = column col
    fold (BinSqlExpr op left right) = bin op (fold left) (fold right)
    fold (PrefixSqlExpr op exp) = prefix op (fold exp)
    fold (PostfixSqlExpr op exp) = postfix op (fold exp)
    fold (FunSqlExpr name exprs) = fun name (map fold exprs)
    fold (AggrFunSqlExpr name exprs) = aggr name (map fold exprs)
    fold (ConstSqlExpr c) = constant c
    fold (CaseSqlExpr cases def) = _case (map (\(e1, e2) -> (fold e1, fold e2)) cases) (fold def)
    fold (ListSqlExpr exprs) = list (map fold exprs)
    fold (ExistsSqlExpr select) = exists select
    fold (ParamSqlExpr name exp) = param name (fold exp)
    fold PlaceHolderSqlExpr = placeHolder
    fold (ParensSqlExpr exp) = parens (fold exp)
    fold (CastSqlExpr typ exp ) = casts typ (fold exp) 

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
                       groupby	 = Nothing,
                       orderby	 = [],
                       extra     = []
                      }

