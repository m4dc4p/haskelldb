-----------------------------------------------------------
-- |
-- Module      :  PrimQuery
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non portable
-- 
-- PrimQuery defines the datatype of relational expressions
-- ('PrimQuery') and some useful functions on PrimQuery\'s
--
-- 
-----------------------------------------------------------
module Database.HaskellDB.PrimQuery (
		  -- * Type Declarations

		  -- ** Types
		  TableName, Attribute, Scheme, Assoc

		  -- ** Data types
		 , PrimQuery(..), RelOp(..), SpecialOp(..) 
		 , PrimExpr(..), OrderExpr(..)
                 , BinOp(..), UnOp(..), OrderOp(..), AggrOp(..)
	         , Literal(..)

		  -- * Function declarations
		 , extend, times
		 , attributes, attrInExpr, attrInOrder
		 , substAttr
		 , isAggregate
		 , foldPrimQuery, foldPrimExpr
		 ) where

import Data.List ((\\), union)
import Control.Exception (assert)
import System.Time (CalendarTime, formatCalendarTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
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
                | Group Assoc PrimQuery
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

data SpecialOp  = Order [OrderExpr]
		| Top Int
		deriving (Show)

data OrderExpr = OrderExpr OrderOp PrimExpr 
		deriving (Show)

data OrderOp = OpAsc | OpDesc
		deriving (Show)

data PrimExpr   = AttrExpr  Attribute
                | BinExpr   BinOp PrimExpr PrimExpr
                | UnExpr    UnOp PrimExpr
                | AggrExpr  AggrOp PrimExpr
                | ConstExpr Literal
		| CaseExpr [(PrimExpr,PrimExpr)] PrimExpr
                | ListExpr [PrimExpr]
                deriving (Read,Show)

data Literal = NullLit
	     | DefaultLit            -- ^ represents a default value
	     | BoolLit Bool
	     | StringLit String
	     | IntegerLit Integer
	     | DoubleLit Double
	     | DateLit CalendarTime
	     | OtherLit String       -- ^ used for hacking in custom SQL
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
		| OpIsNull | OpIsNotNull
		| OpLength
		| UnOpOther String
		deriving (Show,Read)

data AggrOp     = AggrCount | AggrSum | AggrAvg | AggrMin | AggrMax
                | AggrStdDev | AggrStdDevP | AggrVar | AggrVarP
                | AggrOther String
                deriving (Show,Read)


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
times query1 query2     = 
    assert (length (attributes query1 \\ attributes query2) == 
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
                                    Times       -> attr1 `union` attr2
                                    Union       -> attr1
                                    Intersect   -> attr1
                                    Divide      -> attr1 
                                    Difference  -> attr1
                                where
                                  attr1         = attributes q1
                                  attr2         = attributes q2
attributes (Group _ qry) = attributes qry

-- | Returns a one-to-one association of a
--   schema. ie. @assocFromScheme ["name","city"]@ becomes:
--   @[("name",AttrExpr "name"), ("city",AttrExpr "city")]@
assocFromScheme :: Scheme -> Assoc
assocFromScheme scheme          
		= map (\attr -> (attr,AttrExpr attr)) scheme


-- | Returns all attributes in an expression.
attrInExpr :: PrimExpr -> Scheme
attrInExpr      = foldPrimExpr (attr,scalar,binary,unary,aggr,_case,list)
                where
                  attr name     = [name]
                  scalar s      = []
                  binary op x y = x ++ y
                  unary op x    = x
                  aggr op x	= x
		  _case cs el   = concat (uncurry (++) (unzip cs)) ++ el
                  list xs       = concat xs

-- | Returns all attributes in a list of ordering expressions.
attrInOrder :: [OrderExpr] -> Scheme
attrInOrder os = concat [attrInExpr e | OrderExpr _ e <- os]

-- | Substitute attribute names in an expression.
substAttr :: Assoc -> PrimExpr -> PrimExpr
substAttr assoc 
    = foldPrimExpr (attr,ConstExpr,BinExpr,UnExpr,AggrExpr,CaseExpr,ListExpr)
        where 
          attr name     = case (lookup name assoc) of
                            Just x      -> x 
                            Nothing     -> AttrExpr name

isAggregate :: PrimExpr -> Bool
isAggregate x = countAggregate x > 0

countAggregate :: PrimExpr -> Int
countAggregate
	= foldPrimExpr (const 0, const 0, binary, unary, aggr, _case, list)
	where
          binary op x y	 	= x + y
          unary op x		= x
          aggr op x		= x + 1
	  _case cs el           = sum (map (uncurry (+)) cs) + el
          list xs               = sum xs

-- | Fold on 'PrimQuery'
foldPrimQuery :: (t, TableName -> Scheme -> t, Assoc -> t -> t,
                  PrimExpr -> t -> t, RelOp -> t -> t -> t,
                  Assoc -> t -> t, SpecialOp -> t -> t) -> PrimQuery -> t
foldPrimQuery (empty,table,project,restrict,binary,group,special) 
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
          fold (Group assocs query)
                        = group assocs (fold query)
          fold (Special op query)
          		= special op (fold query)
-- | Fold on 'PrimExpr'
foldPrimExpr :: (Attribute -> t, Literal -> t, BinOp -> t -> t -> t,
                 UnOp -> t -> t, AggrOp -> t -> t, 
		 [(t,t)] -> t -> t, [t] -> t) -> PrimExpr -> t
foldPrimExpr (attr,scalar,binary,unary,aggr,_case,list) 
        = fold
        where
          fold (AttrExpr name) = attr name
          fold (ConstExpr s)   = scalar s
          fold (BinExpr op x y)= binary op (fold x) (fold y)
          fold (UnExpr op x)   = unary op (fold x)
          fold (AggrExpr op x) = aggr op (fold x)
	  fold (CaseExpr cs el) = _case (map (both fold) cs) (fold el)
          fold (ListExpr xs) = list (map fold xs)

          both f (x,y) = (f x, f y)
