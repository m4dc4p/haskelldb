-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.Sql.Print
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
-- 
-- Pretty-print SQL
-- 
-----------------------------------------------------------
module Database.HaskellDB.Sql.Print ( 
	                             ppSql, 
                                     ppUpdate,
                                     ppDelete, 
                                     ppInsert, 
                                     ppCreate,
                                     ppDrop,
                                     ppSqlExpr
	                            ) where

import Database.HaskellDB.Sql

import Data.List (intersperse)
import Text.PrettyPrint.HughesPJ


-- * SELECT

-- | Pretty prints a 'SqlSelect'
ppSql :: SqlSelect -> Doc
ppSql (SqlSelect options attrs tables criteria groupby orderby extra)
    = text "SELECT" 
      <+> hsep (map text options)
      <+> ppAttrs attrs
      $$ ppTables tables
      $$ ppWhere criteria
      $$ maybe empty ppGroupBy groupby
      $$ ppOrderBy orderby
      $$ hsep (map text extra)
ppSql (SqlBin op q1 q2) = parens (ppSql q1) $$ text op $$ parens (ppSql q2)
ppSql (SqlTable name)   = text name
ppSql (SqlEmpty)        = text ""

ppAttrs :: [(SqlColumn,SqlExpr)] -> Doc
ppAttrs [] = text "*"
ppAttrs xs = commaV nameAs xs
    where
      -- | Print a name-value binding, or just the name if
      --   name and value are the same.
      nameAs :: (SqlColumn,SqlExpr) -> Doc
      nameAs (name, ColumnSqlExpr c) | name == c = text name
      nameAs (name,expr) = ppAs name (ppSqlExpr expr)

-- FIXME: table aliases start from 1 in every select, which means that
-- with binary RelOps we can get table alias clashes.
ppTables :: [(SqlTable,SqlSelect)] -> Doc
ppTables [] = empty
ppTables ts = text "FROM" <+> commaV ppTable (zipWith tableAlias [1..] ts)
  where
    tableAlias :: Int -> (SqlTable,SqlSelect) -> (SqlTable,SqlSelect)
    tableAlias i (_,sql)  		= ("T" ++ show i,sql)

    ppTable :: (SqlTable,SqlSelect) -> Doc
    ppTable (alias,(SqlTable name)) = ppAs alias (text name)
    ppTable (alias,sql)             = ppAs alias (parens (ppSql sql))

ppWhere :: [SqlExpr] -> Doc
ppWhere [] = empty
ppWhere es = text "WHERE" 
             <+> hsep (intersperse (text "AND") (map ppSqlExpr es))

ppGroupBy :: Mark -> Doc
ppGroupBy All = error "Should not ever print GroupBy all."
ppGroupBy (Columns es) = text "GROUP BY" <+> ppGroupAttrs es
  where
    ppGroupAttrs :: [(SqlColumn, SqlExpr)] -> Doc
    ppGroupAttrs cs = commaV nameOrExpr cs
    nameOrExpr :: (SqlColumn, SqlExpr) -> Doc
    nameOrExpr (_, ColumnSqlExpr col) = text col
    nameOrExpr (_, expr) = parens (ppSqlExpr expr)
    
ppOrderBy :: [(SqlExpr,SqlOrder)] -> Doc
ppOrderBy [] = empty
ppOrderBy ord = text "ORDER BY" <+> commaV ppOrd ord
    where
      ppOrd (e,o) = ppSqlExpr e <+> ppSqlOrder o
      ppSqlOrder :: SqlOrder -> Doc
      ppSqlOrder SqlAsc = text "ASC"
      ppSqlOrder SqlDesc = text "DESC"

ppAs :: String -> Doc -> Doc
ppAs alias expr    | null alias    = expr                               
                   | otherwise     = expr <+> (hsep . map text) ["as",alias]


-- * UPDATE

-- | Pretty prints a 'SqlUpdate'
ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate name assigns criteria)
        = text "UPDATE" <+> text name
        $$ text "SET" <+> commaV ppAssign assigns
        $$ ppWhere criteria
    where
      ppAssign (c,e) = text c <+> equals <+> ppSqlExpr e


-- * DELETE

-- | Pretty prints a 'SqlDelete'
ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete name criteria) =
    text "DELETE FROM" <+> text name $$ ppWhere criteria


-- * INSERT

ppInsert :: SqlInsert -> Doc

ppInsert (SqlInsert table names values)
    = text "INSERT INTO" <+> text table 
      <+> parens (commaV text names)
      $$ text "VALUES" <+> parens (commaV ppSqlExpr values)

ppInsert (SqlInsertQuery table names select)
    = text "INSERT INTO" <+> text table
      <+> parens (commaV text names)
      $$ ppSql select


-- * CREATE

-- | Pretty prints a 'SqlCreate'. 
ppCreate :: SqlCreate -> Doc
ppCreate (SqlCreateDB name) = text "CREATE DATABASE" <+> text name
ppCreate (SqlCreateTable name xs) 
    = text "CREATE TABLE" <+> text name 
      <+> parens (commaV ppF xs)
    where
    ppF (fname,t) = text fname <+> ppSqlTypeNull t

ppSqlTypeNull :: (SqlType,Bool) -> Doc
ppSqlTypeNull (t,nullable) = ppSqlType t <+> text (if nullable then " null" else " not null")

ppSqlType :: SqlType -> Doc
ppSqlType (SqlType t) = text t
ppSqlType (SqlType1 t x) = text t <> parens (int x)
ppSqlType (SqlType2 t x y) = text t <> parens (commaH int [x,y])


-- * DROP

-- | Pretty prints a 'SqlDrop'.
ppDrop :: SqlDrop -> Doc
ppDrop (SqlDropDB name) = text "DROP DATABASE" <+> text name
ppDrop (SqlDropTable name) = text "DROP TABLE" <+> text name 


-- * Expressions

-- | Pretty prints a 'SqlExpr'
ppSqlExpr :: SqlExpr -> Doc
ppSqlExpr e =
    case e of
      ColumnSqlExpr c     -> text c
      ParensSqlExpr e -> parens (ppSqlExpr e)
      BinSqlExpr op e1 e2 -> ppSqlExpr e1 <+> text op <+> ppSqlExpr e2 
      PrefixSqlExpr op e  -> text op <+> ppSqlExpr e
      PostfixSqlExpr op e -> ppSqlExpr e <+> text op
      FunSqlExpr f es     -> text f <> parens (commaH ppSqlExpr es)
      AggrFunSqlExpr f es     -> text f <> parens (commaH ppSqlExpr es)
      ConstSqlExpr c      -> text c
      CaseSqlExpr cs el   -> text "CASE" <+> vcat (map ppWhen cs)
                             <+> text "ELSE" <+> ppSqlExpr el <+> text "END"
          where ppWhen (w,t) = text "WHEN" <+> ppSqlExpr w 
                               <+> text "THEN" <+> ppSqlExpr t
      ListSqlExpr es      -> parens (commaH ppSqlExpr es)
      ExistsSqlExpr s     -> text "EXISTS" <+> parens (ppSql s)
      ParamSqlExpr n v -> ppSqlExpr v
      PlaceHolderSqlExpr -> text "?"
      CastSqlExpr typ expr -> text "CAST" <> parens (ppSqlExpr expr <+> text "AS" <+> text typ)
    

commaH :: (a -> Doc) -> [a] -> Doc
commaH f = hcat . punctuate comma . map f

commaV :: (a -> Doc) -> [a] -> Doc
commaV f = vcat . punctuate comma . map f