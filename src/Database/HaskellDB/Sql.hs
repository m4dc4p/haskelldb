-----------------------------------------------------------
-- |
-- Module      :  SQL
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Transform a PrimQuery (relational expression) to SQL
-- and pretty print SQL
-----------------------------------------------------------
module Database.HaskellDB.Sql ( 
		-- * Type Declarations
	     SqlSelect(..) 
	   , SqlUpdate(..) 
	   , SqlDelete(..) 
	   , SqlInsert(..)
	   -- * Function Declarations
	   , toSql, ppSql
	   , toUpdate, ppUpdate
	   , toDelete, ppDelete
	   , toInsert, ppInsert
	   , toInsertQuery
	   ) where

import Data.List (intersect)
import Database.HaskellDB.PrimQuery

import Text.PrettyPrint.HughesPJ

-----------------------------------------------------------
-- SQL data type
-----------------------------------------------------------

data SqlSelect  = SqlSelect   { options  :: [String]
			      , attrs    :: [(Attribute,String)]
                              , tables   :: [(TableName,SqlSelect)]
                              , criteria :: [String]
                              , groupby  :: [String]
                              , orderby	 :: [PrimExpr]
                              }
                | SqlBin   String SqlSelect SqlSelect
                | SqlTable TableName
                | SqlEmpty


data SqlUpdate  = SqlUpdate TableName [String] [String]

data SqlDelete  = SqlDelete TableName [String]

data SqlInsert  = SqlInsert  TableName [(Attribute,String)]
                | SqlInsertQuery TableName SqlSelect



newSelect       = SqlSelect { options   = []
			    , attrs 	= []
			    , tables 	= []
			    , criteria 	= []
			    , groupby	= []
			    , orderby	= [] }


-----------------------------------------------------------
-- SELECT
-- Hmm, bit messy.
-----------------------------------------------------------

-- invariant: null attrs => null groupby

toSql   :: PrimQuery -> SqlSelect
toSql   = foldPrimQuery (empty,table,project,restrict,binary,special)
        where
          empty             	= SqlEmpty
          table name schema 	= SqlTable name
	 
          project assoc q
          	| hasAggr    = select { groupby = map toSqlExpr nonAggrs }
          	| otherwise  = select 
                where
                  select   = sql { attrs = toSqlAssoc assoc }
                  sql      = toSelect q

                  hasAggr  = any isAggregate exprs
                  
                  -- TODO: we should make sure that every non-aggregate 
                  -- is only a simple attribute expression
                  nonAggrs = filter (not.isAggregate) exprs
                  
                  exprs    = map snd assoc
		  
          restrict expr q
                = sql { criteria = toSqlExpr expr : criteria sql }
                where
                  sql   = toSelect q
                  
          -- binary assumes that q1 and q2 are not empty
          binary Times q1 q2  
          	| null (attrs q1) = addTable q1 q2
          	| null (attrs q2) = addTable q2 q1
          	| otherwise       = newSelect { tables = [("",q1),("",q2)] }
          	where
          	  addTable sql q  = sql{ tables = tables sql ++ [("",q)] }
		 
          binary op q1 q2         
          	= SqlBin (toSqlOp op) q1 q2


	  special (Order newOrder) q
	  	= sql { orderby = newOrder ++ oldOrder }  
		where
		  sql 	    = toSelect q
		  
		  oldOrder  = filter notdup (orderby sql)
		  notdup x  = null (attrInExpr x `intersect` attrInOrder 
				    newOrder)
		  
		  	    
		  	    
          special op q
          	= sql { options = show (ppSpecialOp op) : options sql }
          	where
                  sql	    = toSelect q

toSelect sql    = case sql of
                    (SqlEmpty)          -> newSelect
                    (SqlTable name)     -> newSelect { tables = [("",sql)] }
                    (SqlBin op q1 q2)   -> newSelect { tables = [("",sql)] }
                    s | null (attrs s) -> sql
                      | otherwise  -> newSelect { tables = [("",sql)] }

toSqlAssoc      = map (\(attr,expr) -> (attr, toSqlExpr expr))
toSqlExpr       = show . ppPrimExpr

toSqlOp Union        = "UNION"
toSqlOp Intersect    = "INTERSECT"
toSqlOp Divide       = "DIVIDE"
toSqlOp Difference   = "MINUS"


-----------------------------------------------------------
-- SELECT, show & pretty print
-----------------------------------------------------------
ppSql :: SqlSelect -> Doc
ppSql (SqlSelect options attrs tables criteria groupby orderby)
    = text "SELECT DISTINCT" <+> (hsep . map text) options <+> ppAttrs attrs
      $$ f "FROM " ppTables tables
      $$ f "WHERE" ppCriteria criteria
      $$ f "GROUP BY" ppGroupBy groupby
      $$ f "ORDER BY" ppOrderBy orderby
    where
    f clause action xs    | null xs       = empty
			  | otherwise     = text clause <+> action xs

ppSql (SqlBin op sql1 sql2)     = ppSql sql1 $$ text op $$ ppSql sql2
ppSql (SqlTable name)           = text name
ppSql (SqlEmpty)                = text ""


-- helpers

ppAttrs []	= text "*"
ppAttrs xs      = vcat $ punctuate comma (map nameAs xs)
                                                                 
ppCriteria      = vcat . punctuate (text " AND ") . map text
ppTables        = vcat . punctuate comma . map ppTable . 
		  zipWith tableAlias [1..]
ppGroupBy	= vcat . punctuate comma  . map text 		
ppOrderBy ord	= ppSpecialOp (Order ord)

tableAlias i (_,sql)  		= ("T" ++ show i,sql)

ppTable (alias,(SqlTable name)) = ppAs alias (text name)
ppTable (alias,sql)             = ppAs alias (parens (ppSql sql))


nameAs (name,expr) | name == expr  = text name
                   | otherwise     = ppAs name (text expr)              
                                                                     
ppAs alias expr    | null alias    = expr                               
                   | otherwise     = expr <+> (hsep . map text) ["as",alias]

-----------------------------------------------------------
-- INSERT
-----------------------------------------------------------
toInsertQuery :: TableName -> PrimQuery -> SqlInsert
toInsertQuery table qtree
	= SqlInsertQuery table (toSql qtree)

toInsert :: TableName -> Assoc -> SqlInsert
toInsert table assoc
	= SqlInsert table (map showExpr assoc)
	where
	  showExpr (attr,expr)	= (attr,show (ppPrimExpr expr))

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


-----------------------------------------------------------
-- DELETE
-----------------------------------------------------------

toDelete :: TableName -> [PrimExpr] -> SqlDelete
toDelete name exprs
        = SqlDelete name (map toSqlExpr exprs)

ppDelete :: SqlDelete -> Doc
ppDelete (SqlDelete name exprs)
        | null exprs    =  text ""
        | otherwise     =  text "DELETE FROM" <+> text name
                        $$ text "WHERE" <+> ppCriteria exprs

-----------------------------------------------------------
-- UPDATE
-----------------------------------------------------------

toUpdate :: TableName -> [PrimExpr] -> Assoc -> SqlUpdate
toUpdate name criteria assigns
        = SqlUpdate name (map toSqlExpr criteria)
        		 (map showAssign assigns)
        where
          showAssign (attr,expr)
          	= attr ++ " = " ++ toSqlExpr expr

ppUpdate :: SqlUpdate -> Doc
ppUpdate (SqlUpdate name criteria assigns)
        = text "UPDATE" <+> text name
        $$ text "SET" <+> (vcat $ punctuate comma (map text assigns))
        $$ f "WHERE" ppCriteria criteria
        where
           f clause action xs   | null xs    = empty
                                | otherwise  = text clause <+> action xs
