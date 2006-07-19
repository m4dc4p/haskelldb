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
-- Transform a PrimQuery (relational expression) to SQL
-- and pretty print SQL
--
-- 
-----------------------------------------------------------
module Database.HaskellDB.Sql.Print ( 
	                             ppSql, 
                                     ppUpdate,
                                     ppDelete, 
                                     ppInsert, 
                                     ppCreate,
                                     ppDrop
	                            ) where

import Database.HaskellDB.FieldType
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Sql

import Text.PrettyPrint.HughesPJ


-- * Types

toSqlType :: FieldType -> String
toSqlType StringT = "text"
toSqlType IntT = "int"
toSqlType IntegerT = "bigint"
toSqlType DoubleT = "double precision"
toSqlType BoolT = "bit"
toSqlType CalendarTimeT = "timestamp"
toSqlType (BStrT a) = "varchar(" ++ show a ++ ")"


-- * SELECT

-- | Pretty prints a 'SqlSelect'
ppSql :: SqlSelect -> Doc
ppSql (SqlSelect options attrs tables criteria groupby orderby limit)
    = text "SELECT DISTINCT" <+> (hsep . map text) options <+> ppAttrs attrs
      $$ f "FROM " ppTables tables
      $$ f "WHERE" ppCriteria criteria
      $$ f "GROUP BY" ppGroupBy groupby
      $$ f "ORDER BY" ppOrderBy orderby
      $$ (hsep . map text) limit
    where
    f clause action xs    | null xs       = empty
			  | otherwise     = text clause <+> action xs

ppSql (SqlBin op sql1 sql2)     = parens (ppSql sql1) 
				  $$ text op 
				  $$ parens (ppSql sql2)
ppSql (SqlTable name)           = text name
ppSql (SqlEmpty)                = text ""

ppAttrs :: [(Attribute,String)] -> Doc
ppAttrs []	= text "*"
ppAttrs xs      = vcat $ punctuate comma (map nameAs xs)

ppCriteria :: [String] -> Doc
ppCriteria      = vcat . punctuate (text " AND ") . map text

-- FIXME: table aliases start from 1 in every select, which means that
-- with binary RelOps we can get table alias clashes.
ppTables :: [(TableName,SqlSelect)] -> Doc
ppTables        = vcat . punctuate comma . map ppTable . 
		  zipWith tableAlias [1..]

ppGroupBy :: [String] -> Doc
ppGroupBy	= vcat . punctuate comma  . map text

ppOrderBy :: [PrimExpr] -> Doc
ppOrderBy ord	= ppSpecialOp (Order ord)

tableAlias :: Int -> (TableName,SqlSelect) -> (TableName,SqlSelect)
tableAlias i (_,sql)  		= ("T" ++ show i,sql)

ppTable :: (TableName,SqlSelect) -> Doc
ppTable (alias,(SqlTable name)) = ppAs alias (text name)
ppTable (alias,sql)             = ppAs alias (parens (ppSql sql))

-- | Print a name-value binding, or just the name if
--   name and value are the same.
nameAs :: (Attribute,String) -> Doc
nameAs (name,expr) | name == expr  = text name
                   | otherwise     = ppAs name (text expr)              

ppAs :: TableName -> Doc -> Doc
ppAs alias expr    | null alias    = expr                               
                   | otherwise     = expr <+> (hsep . map text) ["as",alias]

-- * INSERT

ppInsert :: SqlInsert -> Doc
ppInsert (SqlInsertQuery table select)
	= text "INSERT INTO" <+> text table
        $$ ppSql select

ppInsert (SqlInsert table exprs)
    = text "INSERT INTO" <+> text table 
      <+> parens (vcat $ punctuate comma (map text names))
      $$ text "VALUES"   <+> parens (vcat $ punctuate comma (map text values))
    where
    (names,values)        = unzip exprs


-- * DELETE

ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete name exprs) =
    text "DELETE FROM" <+> text name $$ f "WHERE" ppCriteria exprs
   where
     f clause action xs | null xs    = empty
                        | otherwise  = text clause <+> action xs

-- * UPDATE

-- | Pretty prints a 'SqlUpdate'
ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate name criteria assigns)
        = text "UPDATE" <+> text name
        $$ text "SET" <+> (vcat $ punctuate comma (map text assigns))
        $$ f "WHERE" ppCriteria criteria
        where
           f clause action xs   | null xs    = empty
                                | otherwise  = text clause <+> action xs

-- * CREATE

-- | Pretty prints a 'SqlCreate'. 
ppCreate :: SqlCreate -> Doc
ppCreate (SqlCreateDB name) = text "CREATE DATABASE" <+> text name
ppCreate (SqlCreateTable name xs) 
    = text "CREATE TABLE" <+> text name 
      <+> parens (vcat $ punctuate comma (map ppF xs))
    where
    ppF (fname,(ftype,nullable)) 
	= text fname <+> text (toSqlType ftype)
	  <> if nullable then text "" else text " not null"

-- * DROP

-- | Pretty prints a 'SqlDrop'.
ppDrop :: SqlDrop -> Doc
ppDrop (SqlDropDB name) = text "DROP DATABASE" <+> text name
ppDrop (SqlDropTable name) 
    = text "DROP TABLE" <+> text name 
