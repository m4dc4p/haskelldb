-----------------------------------------------------------
-- |
-- Module      :  Query
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  non portable
-- 
-- Basic combinators for building type-safe queries.
-- The Query monad constructs a relational expression
-- ('PrimQuery'). 
--
-- $Revision: 1.56 $
-----------------------------------------------------------
module Database.HaskellDB.Query (
	      -- * Data and class declarations
	      Rel(..), Attr(..), Table(..), Query, Expr(..)
	     , ToPrimExprs, ShowConstant
	     , ExprC, ProjectRec, InsertRec
	      -- * Operators
	     , (.==.) , (.<>.), (.<.), (.<=.), (.>.), (.>=.)
	     , (.&&.) , (.||.)
	     , (.*.) , (./.), (.%.), (.+.), (.-.), (.++.)
             , (<<)
	      -- * Function declarations
	     , runQuery, runQueryRel
	     , attribute, project, baseTable
	     , attributeName, exprs, labels
	     , restrict, table
	     , union, intersect, divide, minus
	     , _not, like, cat, _length
	     , isNull, notNull
	     , fromNull
	     , constant, constJust
	     , count, _sum, _max, _min, avg
	     , stddev, stddevP, variance, varianceP
	     , asc, desc, order
	     , top --, topPercent
             , _case
	     , _default
	     ) where

import Database.HaskellDB.HDBRec
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.BoundedString
import Database.HaskellDB.BoundedList

import System.Time (CalendarTime)

-----------------------------------------------------------
-- Operators
-----------------------------------------------------------

--infix   9 !
infix   8 `like`
infixl  7 .*., ./., .%.
infixl  6 .+.,.-.
infix   6 <<
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

-- | Type of normal expressions, contains the untyped PrimExpr.
data Expr a     = Expr PrimExpr
		deriving (Read, Show)

-- | Type of aggregate expressions.
data ExprAggr a = ExprAggr PrimExpr deriving (Read, Show)

-- | The type of default expressions.
data ExprDefault a = ExprDefault PrimExpr deriving (Read, Show)

-- | Basic tables, contains table name and an
--   association from attributes to attribute
--   names in the real table.
data Table r    = Table TableName Assoc

-- | Typed attributes
data Attr f a   = Attr Attribute

type Alias      = Int

-- | A Query monad provides unique names (aliases)
--   and constructs a PrimQuery.
type QState     = (Alias,PrimQuery)

data Query a    = Query (QState -> (a,QState))


scheme :: Rel r -> Scheme
scheme (Rel _ s) = s

attributeName :: Attr f a -> Attribute
attributeName (Attr name) = name

-----------------------------------------------------------
-- Expression and record classes.
-----------------------------------------------------------

-- | Class of expression types.
class ExprC e where
    -- | Get the underlying untyped 'PrimExpr'.
    primExpr :: e a -> PrimExpr
instance ExprC Expr where primExpr (Expr e) = e
instance ExprC ExprAggr where primExpr (ExprAggr e) = e
instance ExprC ExprDefault where primExpr (ExprDefault e) = e

-- | Class of expressions that can be used with 'insert'.
class ExprC e => InsertExpr e
instance InsertExpr Expr
instance InsertExpr ExprDefault

-- | Class of records that can be used with 'insert'. 
--   All all the values must be instances of 'InsertExpr' for the
--   record to be an instance of 'InsertRec'.
class InsertRec r er | r -> er
instance InsertRec RecNil RecNil
instance (InsertExpr e, InsertRec r er) => 
    InsertRec (RecCons f (e a) r) (RecCons f (Expr a) er)

-- | Class of expressions that can be used with 'project'.
class ExprC e => ProjectExpr e
instance ProjectExpr Expr
instance ProjectExpr ExprAggr

-- | Class of records that can be used with 'project'. 
--   All all the values must be instances of 'ProjectExpr' for the
--   record to be an instance of 'ProjectRec'.
class ProjectRec r er | r -> er
instance ProjectRec RecNil RecNil
instance (ProjectExpr e, ProjectRec r er) => 
    ProjectRec (RecCons f (e a) r) (RecCons f (Expr a) er)

-----------------------------------------------------------
-- Record operators
-----------------------------------------------------------

-- | Creates a record field.
--   Similar to '(.=.)', but gets the field label from an 'Attr'.
( << ) :: Attr f a        -- ^ Label
       -> e a                        -- ^ Expression
       -> Record (RecCons f (e a) RecNil)  -- ^ New record
_ << x = RecCons x

-----------------------------------------------------------
-- Basic relational operators
-----------------------------------------------------------

-- | Field selection operator. It is overloaded to work for both
--   relations in a query and the result of a query.
--   That is, it corresponds to both '!' and '!.' from the original
--   HaskellDB. An overloaded operator was selected because users
--   (and the developers) always forgot to use !. instead of !
--   on query results.
instance HasField f r => Select (Attr f a) (Rel r) (Expr a) where
    rel ! attr = select attr rel

select :: HasField f r => Attr f a -> Rel r -> Expr a
select (Attr attribute) (Rel alias scheme)
        = Expr (AttrExpr (fresh alias attribute))

-- | Specifies a subset of the columns in the table.
project :: (ShowRecRow r, ToPrimExprs r, ProjectRec r er) => Record r -> Query (Rel er)
project r
        = do
	  alias <- newAlias
          let scheme        = labels r
	      assoc         = zip (map (fresh alias) scheme) (exprs r)
	  updatePrimQuery (extend assoc)
          return (Rel alias scheme)


-- | Restricts the records to only those who evaluates the 
-- expression to True.
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

-- | Return all records which are present in at least
--   one of the relations.
union :: Query (Rel r) -> Query (Rel r) -> Query (Rel r)
union           = binrel Union

-- | Return all records which are present in both relations.
intersect :: Query (Rel r) -> Query (Rel r) -> Query (Rel r) 
intersect       = binrel Intersect

-- | Not in SQL92.
divide :: Query (Rel r) -> Query (Rel r) -> Query (Rel r) 
divide          = binrel Divide

-- | Return all records from the first relation that are not 
--   present in the second relation.
minus :: Query (Rel r) -> Query (Rel r) -> Query (Rel r)
minus           = binrel Difference

-----------------------------------------------------------
-- Tables
-----------------------------------------------------------

-- | Return all records from a specific table.
table :: (ShowRecRow r) => Table r -> Query (Rel r)
table (Table name assoc)
        = do
	  alias <- newAlias
          let newAssoc = map (\(attr,expr) -> (fresh alias attr,expr)) assoc
	      scheme   = map fst assoc
	      q        = Project newAssoc (BaseTable name scheme)
	  updatePrimQuery (times q)
          return (Rel alias scheme)

-- used in table definitions

baseTable :: (ShowRecRow r, ToPrimExprs r) => TableName -> Record r -> Table r
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
-- in the same position relavtive to the given characters and
-- _ is a wildcard representing one character e.g.
--
-- > like (constant "ABCDEFFF") (constant "AB%F_F")
-- 
-- is true while
-- 
-- > like (constant "ABCDEF") (constant "AC%F") 
-- 
-- is false.
like :: Expr String -> Expr String -> Expr Bool
like   = binop OpLike


-- | Produces the concatenation of two String-expressions.
cat :: Expr String -> Expr String -> Expr String
cat = binop OpCat

-- | Concatenates two String-expressions. 
(.++.) :: Expr String -> Expr String -> Expr String
(.++.) = cat

-- | Gets the length of a string.
_length :: Expr String -> Expr Int
_length = unop OpLength

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

-- | Creates a conditional expression.
--   Returns the value of the expression corresponding to the first 
--   true condition. If none of the conditions are true, the value of
--   the else-expression is returned.
_case :: [(Expr Bool, Expr a)] -- ^ A list of conditions and expressions.
      -> Expr a                -- ^ Else-expression. 
      -> Expr a
_case cs (Expr el) = Expr (CaseExpr [ (c,e) | (Expr c, Expr e) <- cs] el)

-- | Takes a default value a and a nullable value. The value is NULL,
--   the default value is returned, otherwise the value itself is returned.
--   Simliar to 'fromMaybe'
fromNull :: Expr a         -- ^ Default value (to be returned for 'Nothing')
	 -> Expr (Maybe a) -- ^ A nullable expression
	 -> Expr a
fromNull d x@(Expr px) = _case [(isNull x, d)] (Expr px)

-----------------------------------------------------------
-- Default values
-----------------------------------------------------------

-- | The default value of the column. Only works with 'insert'.
_default :: ExprDefault a
_default = ExprDefault (ConstExpr DefaultLit)

-----------------------------------------------------------
-- Constants
-- Maybe we should change the set according to the 
-- database backend
-----------------------------------------------------------

class ShowConstant a where
    showConstant :: a -> Literal

instance ShowConstant String where
    showConstant = StringLit
instance ShowConstant Int where
    showConstant = IntegerLit . fromIntegral
instance ShowConstant Integer where
    showConstant = IntegerLit
instance ShowConstant Double where
    showConstant = DoubleLit
instance ShowConstant Bool where
    showConstant = BoolLit

-- this assumes that all databases accept both date and time even when they
-- only want date.
instance ShowConstant CalendarTime where
    showConstant = DateLit

instance ShowConstant a => ShowConstant (Maybe a) where
    showConstant = maybe NullLit showConstant

instance Size n => ShowConstant (BoundedString n) where
    showConstant = showConstant . fromBounded

-- | Transform an a into an Expr a.  
constant :: ShowConstant a => a -> Expr a
constant x  = Expr (ConstExpr (showConstant x))

-- | Turn constant data into a nullable expression. 
--   Same as @constant . Just@
constJust :: ShowConstant a => a -> Expr (Maybe a)
constJust x = constant (Just x)

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
aggregate :: HasField f r => AggrOp -> Rel r -> Attr f a -> Expr b
aggregate op rel attr
		= Expr (AggrExpr op primExpr)
		where
 	  	  (Expr primExpr)  = rel ! attr

count :: HasField f r => Rel r -> Attr f a -> Expr Int
count x		= aggregate AggrCount x


numAggregate :: (Num a,HasField f r) => AggrOp -> Rel r -> Attr f a -> Expr a
numAggregate	= aggregate

_sum,_max,_min,avg,stddev,stddevP,variance,varianceP 
    :: (Num a,HasField f r) => Rel r -> Attr f a -> Expr a
_sum x          = numAggregate AggrSum x
_max x          = numAggregate AggrMax x
_min x          = numAggregate AggrMin x
avg x           = numAggregate AggrAvg x
stddev x        = numAggregate AggrStdDev x
stddevP x       = numAggregate AggrStdDevP x
variance x      = numAggregate AggrVar x
varianceP x     = numAggregate AggrVarP x
-}

aggregate :: AggrOp -> Expr a -> ExprAggr b
aggregate op (Expr primExpr) = ExprAggr (AggrExpr op primExpr)

-- | Returns the number of records (=rows) in a query.
count :: Expr a -> ExprAggr Int
count x		= aggregate AggrCount x

-- | Returns the total sum of a column.
_sum :: Num a => Expr a -> ExprAggr a
_sum x = aggregate AggrSum x

-- | Returns the highest value of a column.
_max :: Num a => Expr a -> ExprAggr a
_max x = aggregate AggrMax x

-- | Returns the lowest value of a column.
_min :: Num a => Expr a -> ExprAggr a
_min x = aggregate AggrMin x

-- | Returns the average of a column.
avg :: Num a => Expr a -> ExprAggr a
avg x = aggregate AggrAvg x

-- | Returns the standard deviation of a column.
stddev :: Num a => Expr a -> ExprAggr a
stddev x = aggregate AggrStdDev x

stddevP :: Num a => Expr a -> ExprAggr a
stddevP x = aggregate AggrStdDevP x

-- | Returns the standard variance of a column.
variance :: Num a => Expr a -> ExprAggr a
variance x = aggregate AggrVar x

varianceP :: Num a => Expr a -> ExprAggr a
varianceP x  = aggregate AggrVarP x

-----------------------------------------------------------
-- Special ops
-----------------------------------------------------------

-- | Return the n topmost records.
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

orderOp :: HasField f r => UnOp -> Rel r -> Attr f a -> Expr Order
orderOp op rel attr = Expr (UnExpr op expr)
    where Expr expr = select attr rel

-- | Use this together with the function 'order' to 
-- create an query orderd ascending.
asc :: HasField f r => Rel r -> Attr f a -> Expr Order
asc rel attr	= orderOp OpAsc rel attr


-- | Use this together with the function 'order' to 
-- create an query orderd descending.
desc :: HasField f r => Rel r -> Attr f a -> Expr Order
desc rel attr	= orderOp OpDesc rel attr

-- | HaskellDB counterpart to the SQL keyword ORDER BY. 
-- Use this with the 'asc' or 'desc' functions to create 
-- an ordered 'Query'.
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

labels :: ShowRecRow r => Record r -> [String]
labels r        = map fst (showRecRow r)


-- Type safe version of exprs. If we use this, we must add
--  ToPrimExprs r to a lot of functions
exprs :: ToPrimExprs r => Record r -> [PrimExpr]
exprs r = toPrimExprs (r RecNil)

class ToPrimExprs r where
    toPrimExprs :: r -> [PrimExpr]

instance ToPrimExprs RecNil where
    toPrimExprs RecNil = []

instance (ExprC e, ToPrimExprs r) => ToPrimExprs (RecCons l (e a) r) where
    toPrimExprs (RecCons e r) = primExpr e : toPrimExprs r

{-

exprs :: ShowRecRow r => Record r -> [PrimExpr]
exprs r         = map (readPrimExpr . snd) (showRecRow r)
                where
                  readPrimExpr s   = case (reads (s "")) of
                                    [(Expr qx,_)] -> qx
                                    _             -> error ("record with invalid expression value: " ++ (s ""))
-}
