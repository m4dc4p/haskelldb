-----------------------------------------------------------
-- Daan Leijen (c) 1999, daan@cs.uu.nl
--
-- Basic combinators for building type-safe queries.
-- The "Query" monad constructs a relational expression
-- (PrimQuery).
-----------------------------------------------------------
module Query (
	      Rel(..), Attr(..), Table(..), Query, Expr(..)
	     , runQuery, runQueryRel
	     , attribute, project, baseTable
	     , attributeName, exprs, labels
	     , (!)
	     , restrict, table
	     , union, intersect, divide, minus
	     , (.==.) , (.<>.), (.<.), (.<=.), (.>.), (.>=.)
	     , (.&&.) , (.||.)
	     , (.*.) , (./.), (.%.), (.+.), (.-.), (.++.)
	     , _not, like, cat
	     , isNull, notNull
	     , constant
	     , count, _sum, _max, _min, avg
	     , stddev, stddevP, variance, varianceP
	     , asc, desc, order
	     , top, topPercent
	     ) where

import HDBRec
import PrimQuery

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


(.==.),(.<>.) :: Eq a => Expr a -> Expr a -> Expr Bool
(.==.) = binop OpEq
(.<>.) = binop OpNotEq

(.<.),(.<=.),(.>.),(.>=.) :: Ord a => Expr a -> Expr a -> Expr Bool
(.<.)  = binop OpLt
(.<=.) = binop OpLtEq
(.>.)  = binop OpGt
(.>=.) = binop OpGtEq

_not :: Expr Bool -> Expr Bool
_not   = unop OpNot

(.&&.),(.||.) :: Expr Bool -> Expr Bool -> Expr Bool
(.&&.) = binop OpAnd
(.||.) = binop OpOr


like :: Expr String -> Expr String -> Expr Bool
like   = binop OpLike

cat,(.++.) :: Expr String -> Expr String -> Expr String
cat    = binop OpCat
(.++.) = cat

numop :: Num a => BinOp -> Expr a -> Expr a -> Expr a
numop   = binop

(.+.),(.-.),(.*.),(./.),(.%.) :: Num a => Expr a -> Expr a -> Expr a
(.+.) = numop OpPlus
(.-.) = numop OpMinus
(.*.) = numop OpMul
(./.) = numop OpDiv
(.%.) = numop OpMod

isNull,notNull :: Expr a -> Expr Bool
isNull  = unop OpIsNull
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

instance ShowConstant a => ShowConstant (Maybe a) where
    showConstant x = maybe "NULL" showConstant x

constant :: ShowConstant a => a -> Expr a
constant x      = Expr (ConstExpr (showConstant x))

nullable        :: ShowConstant a => a -> Expr (Maybe a)
nullable x      = Expr (ConstExpr (showConstant x))


-----------------------------------------------------------
-- Aggregate operators
-----------------------------------------------------------

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

-----------------------------------------------------------
-- Special ops
-----------------------------------------------------------

top,topPercent :: Integer -> Query ()
top n           = updatePrimQuery_ (Special (Top False n))
topPercent n    = updatePrimQuery_ (Special (Top True perc))
                where
                  perc  | n < 0         = 0
                        | n > 100       = 100
                        | otherwise     = n

data Order	= OrderPhantom

orderOp :: UnOp -> Rel r -> Attr f r a -> Expr Order
orderOp op rel attr = Expr (UnExpr op expr)
	where
	  (Expr expr) = rel ! attr

asc,desc :: Rel r -> Attr f r a -> Expr Order
asc rel attr	= orderOp OpAsc rel attr
desc rel attr	= orderOp OpDesc rel attr

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
