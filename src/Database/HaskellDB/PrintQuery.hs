-----------------------------------------------------------
-- |
-- Module      :  PrintQuery.hs
-- Copyright   :  haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non portable
-- Author      :  Justin Bailey (jgbailey AT gmail DOT com)
-- Pretty printing for Query, PrimQuery, and SqlSelect values.
-- Useful for debugging the library.
-- 
-----------------------------------------------------------
module Database.HaskellDB.PrintQuery 
    (ppQuery, ppQueryUnOpt
    , ppSelect, ppSelectUnOpt, ppSqlSelect, ppPrim
    , Database.HaskellDB.PrintQuery.ppSql, Database.HaskellDB.PrintQuery.ppSqlUnOpt)

where

import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Sql
import Database.HaskellDB.Query (Query, runQuery, Rel)
import Database.HaskellDB.Optimize (optimize)
import Database.HaskellDB.Sql.Generate (sqlQuery)
import Database.HaskellDB.Sql.Default (defaultSqlGenerator)
import Database.HaskellDB.Sql.Print as Sql (ppSql)
import Text.PrettyPrint.HughesPJ

-- | Take a query, turn it into a SqlSelect and print it.
ppSql :: Query (Rel r) -> Doc
ppSql qry = Sql.ppSql . sqlQuery defaultSqlGenerator . optimize $ runQuery qry

-- | Take a query, turn it into a SqlSelect and print it.
ppSqlUnOpt :: Query (Rel r) -> Doc
ppSqlUnOpt qry = Sql.ppSql . sqlQuery defaultSqlGenerator $ runQuery qry

-- | Take a query, turn it into a SqlSelect and print it.
ppSelect :: Query (Rel r) -> Doc
ppSelect qry = ppPQ (sqlQuery defaultSqlGenerator) optimize (runQuery $ qry)

-- | Take a query, turn it into a SqlSelect and print it, with optimizations.
ppSelectUnOpt :: Query (Rel r) -> Doc
ppSelectUnOpt qry = ppPQ (sqlQuery defaultSqlGenerator) id (runQuery $ qry)

-- | Optimize the query and pretty print the primitive representation.
ppQuery :: Query (Rel r) -> Doc
ppQuery qry = ppPrimF optimize (runQuery $ qry)

-- | Pretty print the primitive representation of an unoptimized query.
ppQueryUnOpt :: Query (Rel r) -> Doc
ppQueryUnOpt qry = ppPrimF id (runQuery $ qry)

-- | Pretty print a PrimQuery value.
ppPrim :: PrimQuery -> Doc
ppPrim = ppPrimF id

-- | Transform a PrimQuery according to the function given, then
-- pretty print it.
ppPrimF :: (PrimQuery -> PrimQuery) -- ^ Transformation function to apply to PrimQuery first.
  -> PrimQuery -- ^ PrimQuery to print.
  -> Doc
ppPrimF f qry = ppPrimF' (f qry)
  where
    ppPrimF' (BaseTable tableName scheme) =
      hang (text "BaseTable" <> colon <+> text tableName)
        nesting
        (brackets (fsep $ punctuate comma (map text scheme)))
    ppPrimF' (Project assoc primQuery) =
      hang (text "Project")
        nesting (brackets (ppAssoc assoc) $+$
        parens (ppPrimF' primQuery))   
    ppPrimF' (Restrict primExpr primQuery) =
      hang (text "Restrict")
        nesting
        (ppExpr primExpr $+$ ppPrimF' primQuery)
    ppPrimF' (Group assoc primQuery) =
      hang (text "Group")
        nesting
        (brackets (ppAssoc assoc) $+$
            parens (ppPrimF' primQuery))
    ppPrimF' (Binary relOp primQueryL primQueryR) =
      hang (text "Binary:" <+> text (show relOp))
        nesting
        (parens (ppPrimF' primQueryL) $+$
          parens (ppPrimF' primQueryR))
    ppPrimF' (Special specialOp primQuery) =
      hang (text "Special:" <+> text (show specialOp))
        nesting
        (parens (ppPrimF' primQuery))
    ppPrimF' Empty = text "Empty"

    -- | Pretty print an Assoc list (i.e. columns and expression).
    ppAssoc :: Assoc -> Doc
    ppAssoc assoc = fsep . punctuate comma . map (\(a, e) -> text a <> colon <+> ppExpr e) $ assoc
    
    -- | Pretty print an PrimExpr value.
    ppExpr :: PrimExpr -> Doc
    ppExpr = text . show

ppPQ :: (PrimQuery -> SqlSelect) -- ^ Function to turn primitive query into a SqlSelect.
  -> (PrimQuery -> PrimQuery) -- ^ Transformation to apply to query, if any.
  -> PrimQuery -- ^ The primitive query to transform and print.
  -> Doc
ppPQ select trans prim = ppSqlSelect . select . trans $ prim

ppSqlSelect :: SqlSelect -> Doc
ppSqlSelect (SqlBin string sqlSelectL sqlSelectR) =
  hang (text "SqlBin:" <+> text string) nesting
    (parens (ppSqlSelect sqlSelectL) $+$
      parens (ppSqlSelect sqlSelectR))
ppSqlSelect (SqlTable sqlTable) = text "SqlTable:" <+> text sqlTable
ppSqlSelect SqlEmpty = text "SqlEmpty"
ppSqlSelect (SqlSelect options attrs tables criteria groupby orderby extra) =
  hang (text "SqlSelect") nesting $
    hang (text "attrs:") nesting (brackets . fsep . punctuate comma . map ppAttr $ attrs) $+$
      text "criteria:" <+> (brackets . fsep . punctuate comma . map ppSqlExpr $ criteria) $+$
      hang (text "tables:") nesting (brackets . fsep . punctuate comma . map ppTable $ tables) $+$
      maybe (text "groupby: empty") ppGroupBy groupby $+$
      hang (text "orderby:") nesting (brackets . fsep . punctuate comma . map ppOrder $ orderby) $+$
      text "extras:" <+> (brackets . fsep. punctuate comma . map text $ extra) $+$
      text "options:" <+> (brackets . fsep . punctuate comma . map text $ options)

ppGroupBy All = text "groupby: all"
ppGroupBy (Columns cs) = hang (text "groupby:") nesting (brackets . fsep . punctuate comma . map ppAttr $ cs)

ppTable :: (SqlTable, SqlSelect) -> Doc
ppTable (tbl, select) =
  if null tbl
    then ppSqlSelect select
    else hang (text tbl <> colon) nesting (ppSqlSelect select)

ppAttr :: (SqlColumn, SqlExpr) -> Doc
ppAttr (col, expr) = text col <> colon <+> ppSqlExpr expr

ppOrder :: (SqlExpr, SqlOrder) -> Doc
ppOrder (expr, order) = parens (ppSqlExpr expr) <+> text (show order)

ppSqlExpr :: SqlExpr -> Doc
ppSqlExpr sql = text $ show sql

-- | Nesting level.
nesting :: Int
nesting = 2