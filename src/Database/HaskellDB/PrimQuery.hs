-----------------------------------------------------------
-- |
-- Module      :  PrimQuery
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
-- 
-- PrimQuery defines the datatype of relational expressions
-- ('PrimQuery') and some useful functions on PrimQuery\'s
-----------------------------------------------------------
module Database.HaskellDB.PrimQuery (
		  -- * Type Declarations

		  -- ** Types
		  TableName, Attribute, Scheme, Assoc

		  -- ** Data types
		 , PrimQuery(..), RelOp(..), SpecialOp(..) 
		 , PrimExpr(..), BinOp(..), UnOp(..), AggrOp(..)

		  -- * Function declarations
		 , extend, times
		 , attributes, attrInExpr, attrInOrder
		 , substAttr
		 , isAggregate, nestedAggregate
		 , foldPrimQuery, foldPrimExpr
		 , assert		 
		 -- ** Pretty printers
		 , ppPrimQuery, ppPrimExpr 
		 , ppRelOp, ppBinOp, ppAggrOp, ppSpecialOp 
		 ) where

import Data.List ((\\))

import Text.PrettyPrint.HughesPJ

-----------------------------------------------------------
-- data definitions
-- PrimQuery is the data type of relational expressions. 
-- Since 'Project' takes an association, it is actually a
-- projection- and rename-operator at once.
-----------------------------------------------------------

type TableName  = String
type Attribute  = String
type Scheme     = [Attribute]
type Assoc      = [(Attribute,PrimExpr)]


data PrimQuery  = BaseTable TableName Scheme
                | Project   Assoc PrimQuery
                | Restrict  PrimExpr PrimQuery
                | Binary    RelOp PrimQuery PrimQuery
                | Special   SpecialOp PrimQuery
                | Empty
		deriving (Show)

data RelOp      = Times 
                | Union 
                | Intersect 
                | Divide 
                | Difference
                deriving (Show)

data SpecialOp  = Order [PrimExpr] -- ^ always UnExpr (OpDesc|OpAsc) (AttrExpr name)
		| Top Bool Integer -- ^ 'True' = top percent, 'False' = top n
		deriving (Show)

data PrimExpr   = AttrExpr  Attribute
                | BinExpr   BinOp PrimExpr PrimExpr
                | UnExpr    UnOp PrimExpr
                | AggrExpr  AggrOp PrimExpr
                | ConstExpr String
                deriving (Read,Show)


data BinOp      = OpEq | OpLt | OpLtEq | OpGt | OpGtEq | OpNotEq 
                | OpAnd | OpOr
                | OpLike | OpIn 
                | OpOther String

                | OpCat
                | OpPlus | OpMinus | OpMul | OpDiv | OpMod
                | OpBitNot | OpBitAnd | OpBitOr | OpBitXor
                | OpAsg
                deriving (Show,Read)

data UnOp	= OpNot 
		| OpAsc | OpDesc
		| OpIsNull | OpIsNotNull
		deriving (Show,Read)

data AggrOp     = AggrCount | AggrSum | AggrAvg | AggrMin | AggrMax
                | AggrStdDev | AggrStdDevP | AggrVar | AggrVarP
                | AggrOther String
                deriving (Show,Read)

-- | Assertions
assert :: String -> String -> String -> Bool -> a -> a
assert moduleName functionName msg test x
 	| test      = x
        | otherwise = error ("assert: " ++ moduleName ++ "." 
        		     ++ functionName ++ ": " ++ msg)

-- | Creates a projection of some attributes while
--   keeping all other attributes in the relation visible too. 
extend :: Assoc -> PrimQuery -> PrimQuery
extend assoc query	
	= Project (assoc ++ assoc') query
        where
          assoc'  = assocFromScheme (attributes query)

-- | Takes the cartesian product of two queries.
times :: PrimQuery -> PrimQuery -> PrimQuery
times (Empty) query	= query
times query (Empty)     = query
times query1 query2     = assert "PrimQuery" "times" "overlapping attributes"
                                 (length (attributes query1 \\ 
					  attributes query2) == 
				  length (attributes query1))
                          Binary Times query1 query2

-- | Returns the schema (the attributes) of a query
attributes :: PrimQuery -> Scheme
attributes (Empty)              = []                            
attributes (BaseTable nm attrs) = attrs
attributes (Project assoc q)    = map fst assoc
attributes (Restrict expr q)    = attributes q
attributes (Special op q)	= attributes q
attributes (Binary op q1 q2)    = case op of
                                    Times       -> attr1 ++ attr2
                                    Union       -> attr1
                                    Intersect   -> attr1 \\ attr2
                                    Divide      -> attr1 
                                    Difference  -> attr1
                                where
                                  attr1         = attributes q1
                                  attr2         = attributes q2

-- | Returns a one-to-one association of a
--   schema. ie. @assocFromScheme ["name","city"]@ becomes:
--   @[("name",AttrExpr "name"), ("city",AttrExpr "city")]@
assocFromScheme :: Scheme -> Assoc
assocFromScheme scheme          
		= map (\attr -> (attr,AttrExpr attr)) scheme


-- | Returns all attributes in an expression.
attrInExpr :: PrimExpr -> Scheme
attrInExpr      = foldPrimExpr (attr,scalar,binary,unary,aggr)
                where
                  attr name     = [name]
                  scalar s      = []
                  binary op x y = x ++ y
                  unary op x    = x
                  aggr op x	= x

-- | Returns all attributes in a list of expressions.
attrInOrder :: [PrimExpr] -> Scheme
attrInOrder  = concat . map attrInExpr

-- | Substitute attribute names in an expression.
substAttr :: Assoc -> PrimExpr -> PrimExpr           
substAttr assoc 
        = foldPrimExpr (attr,ConstExpr,BinExpr,UnExpr,AggrExpr)
        where 
          attr name     = case (lookup name assoc) of
                            Just x      -> x 
                            Nothing     -> AttrExpr name


isAggregate, nestedAggregate :: PrimExpr -> Bool
isAggregate x		= countAggregate x > 0
nestedAggregate x	= countAggregate x > 1

countAggregate :: PrimExpr -> Int
countAggregate
	= foldPrimExpr (const 0, const 0, binary, unary, aggr)
	where
          binary op x y	 	= x + y
          unary op x		= x
          aggr op x		= x + 1

-- | Fold on 'PrimQuery'
foldPrimQuery :: (t, TableName -> Scheme -> t, Assoc -> t -> t,
                  PrimExpr -> t -> t, RelOp -> t -> t -> t,
                  SpecialOp -> t -> t) -> PrimQuery -> t
foldPrimQuery (empty,table,project,restrict,binary,special) 
        = fold
        where
          fold (Empty)  = empty
          fold (BaseTable name schema)
                        = table name schema
          fold (Project assoc query)
                        = project assoc (fold query)
          fold (Restrict expr query)
                        = restrict expr (fold query)
          fold (Binary op query1 query2)
                        = binary op (fold query1) (fold query2)
          fold (Special op query)
          		= special op (fold query)
-- | Fold on 'PrimExpr'
foldPrimExpr :: (Attribute -> t, String -> t, BinOp -> t -> t -> t,
                 UnOp -> t -> t, AggrOp -> t -> t) -> PrimExpr -> t
foldPrimExpr (attr,scalar,binary,unary,aggr) 
        = fold
        where
          fold (AttrExpr name) = attr name
          fold (ConstExpr s)   = scalar s
          fold (BinExpr op x y)= binary op (fold x) (fold y)
          fold (UnExpr op x)   = unary op (fold x)
          fold (AggrExpr op x) = aggr op (fold x)

-----------------------------------------------------------
-- Pretty print PrimQuery and PrimExpr.
-- coincidently, ppPrimExpr shows exactly a valid SQL expression :-)
-----------------------------------------------------------

-- | Pretty prints a 'PrimQuery'
ppPrimQuery :: PrimQuery -> Doc
ppPrimQuery = foldPrimQuery (empty,table,project,restrict,binary,special)
        where
          ontop d e             = nest 2 (d $$ e)
          
          empty                 = empty
          table name scheme     = (hsep . map text) ["BaseTable",name] 
				  <+> ppScheme scheme
          project assoc         = ontop $ text "Project" <+> ppAssoc assoc
          restrict x            = ontop $ text "Restrict" <+> ppPrimExpr x
          binary op d1 d2       = nest 2 (ppRelOp op $$ (d1 $$ d2))
          special op 		= ontop $ ppSpecialOp op

-- | Pretty prints a Scheme
ppScheme :: Scheme -> Doc                    
ppScheme                        = braces . vcat . punctuate comma . map text

-- | Pretty prints an 'Assoc'
ppAssoc :: Assoc -> Doc
ppAssoc                         = braces . vcat . punctuate comma . 
				  map ppNameExpr

-- | Pretty prints ('Attribute', 'PrimExpr')
ppNameExpr :: (Attribute, PrimExpr) -> Doc
ppNameExpr (attr,expr)          = text attr <> colon <+> ppPrimExpr expr

-- | Pretty prints a 'PrimExpr'
ppPrimExpr :: PrimExpr -> Doc
ppPrimExpr = foldPrimExpr (attr,scalar,binary,unary,aggr)
        where
          attr          = text
          scalar        = text . unquote 
          binary op x y = parens (x <+> ppBinOp op <+> y)
          -- paranthesis around ASC / desc exprs not allowed
          unary OpAsc x  = x <+> ppUnOp OpAsc
          unary OpDesc x = x <+> ppUnOp OpDesc
	  unary op x | isPrefixOp op = parens (ppUnOp op <+> x)
		     | otherwise     = parens (x <+> ppUnOp op)

          aggr op x	= ppAggrOp op <> parens x
          
          -- be careful when showing a SQL string
          unquote ('"':s)       = "'" ++ (concat (map tosquote (init s))) 
		                  ++ "'"
          unquote s             = s
          
          tosquote '\''         = "\\'"
          tosquote c            = [c]

	  isPrefixOp OpNot      = True
	  isPrefixOp _          = False

-- PP on ops:
-- | Pretty pints a 'RelOp'
ppRelOp :: RelOp -> Doc
ppRelOp  op		= text (showRelOp  op) 

-- | Pretty pints an 'UnUp'
ppUnOp :: UnOp -> Doc
ppUnOp	 op		= text (showUnOp   op)         

-- | Pretty prints a 'BinOp'
ppBinOp :: BinOp -> Doc
ppBinOp  op             = text (showBinOp  op)

-- | Pretty prints an 'AggrOp'
ppAggrOp :: AggrOp -> Doc
ppAggrOp op             = text (showAggrOp op)

-- | Pretty prints a 'Special Op'
ppSpecialOp :: SpecialOp -> Doc
ppSpecialOp (Order xs)  = (vcat . punctuate comma) (map ppPrimExpr xs)
ppSpecialOp (Top False n)= text "LIMIT" <+> text (show n)
-- FIXME: should we remove topPercent?
-- doesn't seem to be any support for it in e.g.g MySQL and PostgreSQL
ppSpecialOp (Top True n) = error "topPercent not supported"

-----------------------------------------------------------
-- Show expression operators, coincidently they show
-- exactly the SQL equivalents 
-----------------------------------------------------------

showRelOp Times		= "TIMES"
showRelOp Union        	= "UNION"
showRelOp Intersect    	= "INTERSECT"
showRelOp Divide       	= "DIVIDE"
showRelOp Difference   	= "MINUS"

showUnOp  OpNot         = "NOT"
showUnOp  OpIsNull      = "IS NULL" 
showUnOp  OpIsNotNull   = "IS NOT NULL" 
showUnOp  OpAsc         = "ASC"
showUnOp  OpDesc        = "DESC"

showBinOp  OpEq         = "=" 
showBinOp  OpLt         = "<" 
showBinOp  OpLtEq       = "<=" 
showBinOp  OpGt         = ">" 
showBinOp  OpGtEq       = ">=" 
showBinOp  OpNotEq      = "<>" 
showBinOp  OpAnd        = "AND"  
showBinOp  OpOr         = "OR" 
showBinOp  OpLike       = "LIKE" 
showBinOp  OpIn         = "IN" 
showBinOp  (OpOther s)  = s
                
showBinOp  OpCat        = "+" 
showBinOp  OpPlus       = "+" 
showBinOp  OpMinus      = "-" 
showBinOp  OpMul        = "*" 
showBinOp  OpDiv        = "/" 
showBinOp  OpMod        = "MOD" 
showBinOp  OpBitNot     = "~" 
showBinOp  OpBitAnd     = "&" 
showBinOp  OpBitOr      = "|" 
showBinOp  OpBitXor     = "^"
showBinOp  OpAsg        = "="

showAggrOp AggrCount    = "COUNT" 
showAggrOp AggrSum      = "SUM" 
showAggrOp AggrAvg      = "AVG" 
showAggrOp AggrMin      = "MIN" 
showAggrOp AggrMax      = "MAX" 
showAggrOp AggrStdDev   = "StdDev" 
showAggrOp AggrStdDevP  = "StdDevP" 
showAggrOp AggrVar      = "Var" 
showAggrOp AggrVarP     = "VarP"                
showAggrOp (AggrOther s)        = s
