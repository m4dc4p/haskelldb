-----------------------------------------------------------
-- |
-- Module      :  Query
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Basic combinators for building type-safe queries.
-- The "Query" monad constructs a relational expression
-- (PrimQuery). 
-----------------------------------------------------------
module Database.HaskellDB.Query (
		 -- * Data declarations
			Rel(..), Attr(..), Table(..), Query, Expr(..)
	      -- * Operators
	     , (!)
	     , (.==.) , (.<>.), (.<.), (.<=.), (.>.), (.>=.)
	     , (.&&.) , (.||.)
	     , (.*.) , (./.), (.%.), (.+.), (.-.), (.++.)
	      -- * Function declarations
	     , runQuery, runQueryRel
	     , attribute, project, baseTable
	     , attributeName, exprs, labels
	     , restrict, table
	     , union, intersect, divide, minus
	     , _not, like, cat
	     , isNull, notNull
	     , constant, nullable
	     , count, _sum, _max, _min, avg
	     , stddev, stddevP, variance, varianceP
	     , asc, desc, order
	     , top --, topPercent
	     ) where

import Database.HaskellDB.HDBRec
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.BoundedString
import Database.HaskellDB.BoundedList

import System.Time

-----------------------------------------------------------
-- Operators
-----------------------------------------------------------

infix   9 !
infixl  7 .*., ./., .%.
infixl  6 .+.,.-.
infixr  5 .++.
infix   4 .==., .<>., .<., .<=., .>., .>=.
infixr  3 .&&.
infixr  2 .||.

----------------------------------------------------------
-- Data definitions. 
----------------------------------------------------------

-- | Type of relations, contains the attributes
--   of the relation and an 'Alias' to which the
--   attributes are renamed in the 'PrimQuery'.
data Rel r      = Rel Alias Scheme

-- | Type of expressions, contains the untyped PrimExpr.
data Expr a     = Expr PrimExpr
		deriving (Read, Show)

-- | Basic tables, contains table name and an
--   association from attributes to attribute
--   names in the real table.
data Table r    = Table TableName Assoc

-- | Typed attributes
data Attr f r a   = Attr Attribute

type Alias      = Int

-- | A Query monad provides unique names (aliases)
--   and constructs a PrimQuery.
type QState     = (Alias,PrimQuery)

data Query a    = Query (QState -> (a,QState))


scheme :: Rel r -> Scheme
scheme (Rel _ s) = s

attributeName :: Attr f r a -> Attribute
attributeName (Attr name) = name

-----------------------------------------------------------
-- Basic relational operators
-----------------------------------------------------------

(!) :: Rel r -> Attr f r a -> Expr a
rel ! attr      = select attr rel

select :: Attr f r a -> Rel r -> Expr a
select (Attr attribute) (Rel alias scheme)
        = Expr (AttrExpr (fresh alias attribute))

project :: (ShowRecRow r) => HDBRec r -> Query (Rel r)
project r
        = do
	  alias <- newAlias
          let scheme        = labels r
	      assoc         = zip (map (fresh alias) scheme) (exprs r)
	  updatePrimQuery (extend assoc)
          return (Rel alias scheme)

restrict :: Expr Bool -> Query ()
restrict (Expr primExpr) = updatePrimQuery_ (Restrict primExpr)

-----------------------------------------------------------
-- Binary operations
-----------------------------------------------------------

binrel :: RelOp -> Query (Rel r) -> Query (Rel r) -> Query (Rel r)
binrel op (Query q1) (Query q2)
  = Query (\(i,primQ) ->
      let (Rel a1 scheme1,(j,primQ1)) = q1 (i,primQ)
          (Rel a2 scheme2,(k,primQ2)) = q2 (j,primQ)

          alias	  = k
          scheme  = scheme1

          assoc1  = zip (map (fresh alias) scheme1)
          		(map (AttrExpr . fresh a1) scheme1)
          assoc2  = zip (map (fresh alias) scheme2)
          		(map (AttrExpr . fresh a2) scheme2)

          r1      = Project assoc1 primQ1
          r2      = Project assoc2 primQ2
          r       = Binary op r1 r2
      in
          (Rel alias scheme,(k+1,times r primQ)) )

union,intersect,divide,minus :: Query (Rel r) -> Query (Rel r) -> Query (Rel r)
union           = binrel Union
intersect       = binrel Intersect
divide          = binrel Divide
minus           = binrel Difference

-----------------------------------------------------------
-- Tables
-----------------------------------------------------------


table :: (ShowRecRow r) => Table r -> Query (Rel r)
table (Table name assoc)
        = do
	  alias <- newAlias
          let newAssoc = map (\(attr,expr) -> (fresh alias attr,expr)) assoc
	      scheme   = map fst assoc
	      q        = Project newAssoc (BaseTable name scheme)
	  updatePrimQuery (times q)
          return (Rel alias scheme)

-- used in table definitions, see 'pubs.hs' for an example

baseTable :: ShowRecRow r => TableName -> HDBRec r -> Table r
baseTable t r   = Table t (zip (labels r) (exprs r))


attribute :: String -> Expr a
attribute name  = Expr (AttrExpr name)


-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------

unop :: UnOp -> Expr a -> Expr b
unop op (Expr primExpr)
                = Expr (UnExpr op primExpr)

binop :: BinOp -> Expr a  -> Expr b -> Expr c
binop op (Expr primExpr1) (Expr primExpr2)
                = Expr (BinExpr op primExpr1 primExpr2)

-- | (.==.) is used in a similar way as the standard op (==) in
-- Haskell and = in SQL, but takes two 'Expr' as arguments and 
-- returns an 'Expr' Bool.
(.==.) :: Eq a => Expr a -> Expr a -> Expr Bool
(.==.) = binop OpEq

-- | (.\<>.) is used in a similar way as the standard op (\/=) in
-- Haskell and \<> in SQL, but takes two 'Expr' as arguments and 
-- returns an 'Expr' Bool.
(.<>.) :: Eq a => Expr a -> Expr a -> Expr Bool
(.<>.) = binop OpNotEq

-- | As with (.==.) and (.\<>.), this op has a standard Haskell
-- op counterpart; (\<) and an SQL counterpart; \<
(.<.) :: Ord a => Expr a -> Expr a -> Expr Bool
(.<.)  = binop OpLt

-- | As with (.==.) and (.\<>.), this op have a standard Haskell
-- op counterpart, (\<=) and an SQL counterpart; <=.
(.<=.) :: Ord a => Expr a -> Expr a -> Expr Bool
(.<=.) = binop OpLtEq

-- | As with (.==.) and (.\<>.), this op have a standard Haskell
-- op counterpart, (>) and an SQL counterpart; >.
(.>.) :: Ord a => Expr a -> Expr a -> Expr Bool
(.>.)  = binop OpGt

-- | As with (.==.) and (.\<>.), this op have a standard Haskell
-- op counterpart, (>=) and an SQL counterpart; >=.
(.>=.) :: Ord a => Expr a -> Expr a -> Expr Bool
(.>=.) = binop OpGtEq

-- | The inverse of an Expr Bool.
_not :: Expr Bool -> Expr Bool
_not   = unop OpNot

-- | \"Logical and\" on 'Expr', similar to the (&&) op in
-- Haskell and AND in SQL.
(.&&.):: Expr Bool -> Expr Bool -> Expr Bool
(.&&.) = binop OpAnd

-- | \"Logical or\" on 'Expr', similar to the (||) op in
-- Haskell and OR in SQL.
(.||.) :: Expr Bool -> Expr Bool -> Expr Bool
(.||.) = binop OpOr

-- | The HaskellDB counterpart to the SQL LIKE keyword.
-- In the expresions, % is a wildcard representing any characters
-- in the same position relavtive to the given characters.
-- i.e.
-- 
-- > like (constant \"ABCDEF\") (constant \"AB%F\")
-- 
-- is true while
-- 
-- > like (constant \"ABCEDF\") (constant \"AC%F\") 
-- 
-- is false.
like :: Expr String -> Expr String -> Expr Bool
like   = binop OpLike


-- | Produces the concatenation of two String-expressions.
cat :: Expr String -> Expr String -> Expr String
cat = binop OpCat

-- | Concatenates two String-expressions. Is the same as  
-- 'cat'.
(.++.) :: Expr String -> Expr String -> Expr String
(.++.) = cat

numop :: Num a => BinOp -> Expr a -> Expr a -> Expr a
numop   = binop


-- | Addition
(.+.) :: Num a => Expr a -> Expr a -> Expr a
(.+.) = numop OpPlus
-- | Subtraction 
(.-.) :: Num a => Expr a -> Expr a -> Expr a
(.-.) = numop OpMinus
-- | Multiplication
(.*.) :: Num a => Expr a -> Expr a -> Expr a
(.*.) = numop OpMul
-- | Division
(./.) :: Num a => Expr a -> Expr a -> Expr a
(./.) = numop OpDiv
-- | Modulo
(.%.) :: Num a => Expr a -> Expr a -> Expr a
(.%.) = numop OpMod

-- | Returns true if the expression is Null.
isNull :: Expr a -> Expr Bool
isNull  = unop OpIsNull

-- | The inverse of 'isNull', returns false
-- if the expression supplied is Null.
notNull :: Expr a -> Expr Bool
notNull = unop OpIsNotNull

-----------------------------------------------------------
-- Constants
-- Maybe we should change the set according to the 
-- database backend
-----------------------------------------------------------

class ShowConstant a where
    showConstant :: a -> String

instance ShowConstant String where
    showConstant x = show x
instance ShowConstant Int where
    showConstant x = show x
instance ShowConstant Integer where
    showConstant x = show x
instance ShowConstant Double where
    showConstant x = show x
instance ShowConstant Bool where
    -- Bools probably correspond to some numeric type (like BIT),
    -- so 0 and 1 seem reasonable to use.
    showConstant False = show 0
    showConstant True = show 1

-- this assumes that all databases accept both date and time even when they
-- only want date.
instance ShowConstant CalendarTime where
    showConstant (CalendarTime {ctYear = y, ctMonth = mo, ctDay = d, 
				ctHour = h, ctMin = mi, ctSec = s})
       = (show y)++"-"++(show mo)++"-"++(show d)++" "++
	 (show h)++":"++(show mi)++":"++(show s)
	 

instance ShowConstant a => ShowConstant (Maybe a) where
    showConstant x = maybe "NULL" showConstant x

instance (Size n) => ShowConstant (BoundedString n) where
    showConstant x = show x

-- | Transform an a into an Expr a.  
constant :: ShowConstant a => a -> Expr a
constant x  = Expr (ConstExpr (showConstant x))

-- | Turn constant data into a nullable expression. 
--   Same as @constant . Just@
nullable :: ShowConstant a => a -> Expr (Maybe a)
nullable x = constant (Just x)

-----------------------------------------------------------
-- Aggregate operators
--
-- I have changed these to take an expression instead of
-- a relation and an attribute, since that seemed 
-- unneccessarily restrictive. I have probably overlooked 
-- something in doing so, so I left the old code commented out.
-- Bjorn Bringert, 2004-01-10
-----------------------------------------------------------

{-
aggregate :: AggrOp -> Rel r -> Attr f r a -> Expr b
aggregate op rel attr
		= Expr (AggrExpr op primExpr)
		where
 	  	  (Expr primExpr)  = rel ! attr

count :: Rel r -> Attr f r a -> Expr Int
count x		= aggregate AggrCount x


numAggregate :: Num a => AggrOp -> Rel r -> Attr f r a -> Expr a
numAggregate	= aggregate

_sum,_max,_min,avg,stddev,stddevP,variance,varianceP 
    :: Num a => Rel r -> Attr f r a -> Expr a
_sum x          = numAggregate AggrSum x
_max x          = numAggregate AggrMax x
_min x          = numAggregate AggrMin x
avg x           = numAggregate AggrAvg x
stddev x        = numAggregate AggrStdDev x
stddevP x       = numAggregate AggrStdDevP x
variance x      = numAggregate AggrVar x
varianceP x     = numAggregate AggrVarP x
-}

aggregate :: AggrOp -> Expr a -> Expr b
aggregate op (Expr primExpr) = Expr (AggrExpr op primExpr)

count :: Expr a -> Expr Int
count x		= aggregate AggrCount x

_sum,_max,_min,avg,stddev,stddevP,variance,varianceP 
    :: Num a => Expr a -> Expr a
_sum x          = aggregate AggrSum x
_max x          = aggregate AggrMax x
_min x          = aggregate AggrMin x
avg x           = aggregate AggrAvg x
stddev x        = aggregate AggrStdDev x
stddevP x       = aggregate AggrStdDevP x
variance x      = aggregate AggrVar x
varianceP x     = aggregate AggrVarP x

-----------------------------------------------------------
-- Special ops
-----------------------------------------------------------

top :: Integer -> Query ()
top n           = updatePrimQuery_ (Special (Top False n))

{-
-- Disabled by Bjorn since the DBs don't seem to support this
topPercent :: Integer -> Query ()
topPercent n    = updatePrimQuery_ (Special (Top True perc))
                where
                  perc  | n < 0         = 0
                        | n > 100       = 100
                        | otherwise     = n
-}

-----------------------------------------------------------
-- Ordering results
-----------------------------------------------------------

data Order	= OrderPhantom

orderOp :: UnOp -> Rel r -> Attr f r a -> Expr Order
orderOp op rel attr = Expr (UnExpr op expr)
	where
	  (Expr expr) = rel ! attr

-- | Use this together with the function 'order' to 
-- create an query orderd ascending.
asc :: Rel r -> Attr f r a -> Expr Order
asc rel attr	= orderOp OpAsc rel attr


-- | Use this together with the function 'order' to 
-- create an query orderd descending.
desc :: Rel r -> Attr f r a -> Expr Order
desc rel attr	= orderOp OpDesc rel attr

-- Maybe the above should take an expression instead of
-- a relation and an attribute, since that seemes
-- unneccessarily restrictive. However this is not really safe
-- since, for example, in SQL you cannot ORDER BY an
-- aggregate expression. Leaving the alternative code below.
-- Bjorn Bringert, 2004-01-20
{-
orderOp :: UnOp -> Expr a -> Expr Order
orderOp op (Expr expr) = Expr (UnExpr op expr)

asc,desc :: Expr a -> Expr Order
asc 	= orderOp OpAsc
desc	= orderOp OpDesc
-}

-- | HaskellDB countherpart to the SQL keyqword ORDER BY. 
-- Use this with the 'asc' or 'desc' functions to create 
-- an orderd 'Query'.
order :: [Expr Order] -> Query ()
order xs	= updatePrimQuery_ (Special (Order (map unExpr xs)))
		where
		  unExpr (Expr x) = x

-----------------------------------------------------------
-- Query Monad
-----------------------------------------------------------

runQuery :: Query (Rel r) -> PrimQuery
runQuery = fst . runQueryRel

runQueryRel :: Query (Rel r) -> (PrimQuery,Rel r)
runQueryRel (Query f)
        = let (Rel alias scheme,(i,primQuery)) = f (1,Empty)
              assoc   = zip scheme (map (AttrExpr . fresh alias) scheme)
          in  (Project assoc primQuery, Rel 0 scheme)


instance Functor Query where
  fmap f (Query g)      = Query (\q0 -> let (x,q1) = g q0  in (f x,q1))

instance Monad Query where
  return x              = Query (\q0 -> (x,q0))
  (Query g) >>= f       = Query (\q0 -> let (x,q1)    = g q0
                                            (Query h) = f x
                                        in  (h q1))

updatePrimQuery :: (PrimQuery -> PrimQuery) -> Query PrimQuery
updatePrimQuery f  = Query (\(i,qt) -> (qt,(i,f qt)))

updatePrimQuery_ :: (PrimQuery -> PrimQuery) -> Query ()
updatePrimQuery_ f = updatePrimQuery f >> return ()

newAlias :: Query Alias
newAlias                = Query (\(i,qt) -> (i,(i+1,qt)))


-- fresh 0 is used in the 'Database' module
fresh :: Alias -> Attribute -> Attribute
fresh 0     attribute   = attribute
fresh alias attribute   = (attribute ++ show alias)



-----------------------------------------------------------
-- Trex
--
-- hacky now, but this should change when we can
-- define fold-like functions over records.
-----------------------------------------------------------

labels :: ShowRecRow r => HDBRec r -> [String]
labels r        = map fst (showRecRow r)

{-
-- Type safe version of exprs. If we use this, we must add
--  ToPrimExprs r to a lot of functions
exprs :: ToPrimExprs r => HDBRec r -> [PrimExpr]
exprs (HDBRec r) = toPrimExprs r

class ToPrimExprs r where
    toPrimExprs :: r -> [PrimExpr]

instance ToPrimExprs HDBRecTail where
    toPrimExprs HDBRecTail = []

instance ToPrimExprs r => ToPrimExprs (HDBRecCons l (Expr a) r) where
    toPrimExprs (HDBRecCons _ (Expr x) r) = x : toPrimExprs r
-}

exprs :: ShowRecRow r => HDBRec r -> [PrimExpr]
exprs r         = map (readPrimExpr . snd) (showRecRow r)
                where
                  readPrimExpr s   = case (reads (s "")) of
                                    [(Expr qx,_)] -> qx
                                    _             -> error ("record with invalid expression value: " ++ (s ""))
