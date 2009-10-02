{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances
  , TypeSynonymInstances #-}
-----------------------------------------------------------
-- |
-- Module      :  Query
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non portable
-- 
-- Basic combinators for building type-safe queries.
-- The Query monad constructs a relational expression
-- ('PrimQuery'). 
--
-- 
-----------------------------------------------------------
module Database.HaskellDB.Query (
	      module Data.HList
	      -- * Data and class declarations
	     , Rel(..), Table(..), Query, Expr(..), OrderExpr
	     , ToPrimExprsOp(..), ShowLabelsOp(..), ConstantRecordOp(..)
	     , ShowConstant, ExprC, ProjectExpr, ProjectRec, InsertRec
	     , ExprAggr(..), ExprDefault(..)
	     , copy, copyAll
	      -- * Operators
	     , (.==.) , (.<>.), (.<.), (.<=.), (.>.), (.>=.)
	     , (.&&.) , (.||.)
	     , mul, (./.), (.+.), (.-.), (.%.), (.++.)
	      -- * Function declarations
	     , project, restrict, table, unique
	     , union, intersect, divide, minus
	     , _not, like, _in, cat, _length
	     , isNull, notNull
	     , fromNull
	     , constant, constJust
	     , param, namedParam, Args, func, constNull, cast
	     , toStr, coerce
	     , count, _sum, _max, _min, avg
	     , literal
	     , stddev, stddevP, variance, varianceP
	     , asc, desc, order
	     , top
	     , _case
	     , _default
	     -- * Internals
	     , runQuery, runQueryRel
	     , subQuery
	     , attribute, tableName, baseTable, emptyTable
	     , exprs, labels, tableRec 
	     , constantRecord
	     ) where

import Data.HList hiding (cast,(.<.),(.-.),(.+.))
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.BoundedString
import Database.HaskellDB.BoundedList

import System.Time (CalendarTime)

-----------------------------------------------------------
-- Operators
-----------------------------------------------------------


infix   8 `like`, `_in`
infixl  7 `mul`, ./., .%.
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

-- | Type of normal expressions, contains the untyped PrimExpr.
newtype Expr a     = Expr PrimExpr
		deriving (Read, Show)

-- | Type of aggregate expressions.
newtype ExprAggr a = ExprAggr PrimExpr deriving (Read, Show)

-- | The type of default expressions.
newtype ExprDefault a = ExprDefault PrimExpr deriving (Read, Show)


-- | Basic tables, contains table name and an
--   association from attributes to attribute
--   names in the real table.
data Table r    = Table TableName Assoc

type Alias      = Int

-- | A Query monad provides unique names (aliases)
--   and constructs a PrimQuery.
type QState     = (Alias,PrimQuery)

data Query a    = Query (QState -> (a,QState))


scheme :: Rel r -> Scheme
scheme (Rel _ s) = s

-----------------------------------------------------------
-- Expression and record classes.
-----------------------------------------------------------

-- | Class of expression types.
class ExprC e where
    -- | Get the underlying untyped 'PrimExpr'.
    primExpr :: e a -> PrimExpr
instance ExprC Expr where primExpr ~(Expr e) = e
instance ExprC ExprAggr where primExpr ~(ExprAggr e) = e
instance ExprC ExprDefault where primExpr ~(ExprDefault e) = e

-- | Class of expressions that can be used with 'insert'.
class ExprC e => InsertExpr e
instance InsertExpr Expr
instance InsertExpr ExprDefault

-- | Class of records that can be used with 'insert'. 
--   All all the values must be instances of 'InsertExpr' for the
--   record to be an instance of 'InsertRec'.
class InsertRec r er | r -> er
instance InsertRec HNil HNil
instance (InsertExpr e, InsertRec r er) => 
    InsertRec (HCons (LVPair f (e a)) r)
              (HCons (LVPair f (Expr a)) er)

-- | Class of expressions that can be used with 'project'.
class ExprC e => ProjectExpr e
instance ProjectExpr Expr
instance ProjectExpr ExprAggr

-- | Class of records that can be used with 'project'. 
--   All all the values must be instances of 'ProjectExpr' for the
--   record to be an instance of 'ProjectRec'.
class ProjectRec r er | r -> er
instance ProjectRec HNil HNil
instance (ProjectExpr e, ProjectRec r er) => 
    ProjectRec (HCons (LVPair f (e a)) r)
               (HCons (LVPair f (Expr a)) r)

-----------------------------------------------------------
-- Record operators
-----------------------------------------------------------

-- | Extend HList's field selection operator .!. to work on relations in
--   a query.
instance (ShowLabel l, HasField l (Record r) (Expr v))
    => HasField l (Rel (Record r)) (Expr v) where

    hLookupByLabel l (Rel alias _) = Expr (AttrExpr (fresh alias (showLabel l)))

-- | Copies the field and value from the record given. Useful for
-- building projections that will re-use the same column
-- name. @copy attr tbl@ is equivalent to:
--
--   @attr .=. (tbl .!. attr)@
--
copy :: (HasField l (Rel r) (Expr a)) => l -> Rel r -> LVPair l (Expr a)
copy l tbl = l .=. (tbl .!. l)

-- | Copies all columns in the relation given. Useful for appending
-- the remaining columns in a table to a projection. For example:
--
-- >   query = do
-- >     tbl <- table some_table
-- >     project $ copyAll tbl
--
-- will add all columns in "some_table" to the query.
copyAll :: (HRLabelSet r
            , HMap (RecAttrOp (Rel (Record r))) ls r
            , RecordLabels r ls) => Rel (Record r) -> Record r
copyAll tbl = mkRecord $ hMap (RecAttrOp tbl) (recordLabels (unRel tbl))
      where
        unRel :: Rel r -> r
        unRel = undefined

-----------------------------------------------------------
-- Basic relational operators
-----------------------------------------------------------

-- | Specifies a subset of the columns in the table.
project :: (RecordLabels r ls, RecordValues r vs
           , HMapOut ShowLabelsOp ls String, HMapOut ToPrimExprsOp vs PrimExpr
           , ProjectRec r er) => Record r -> Query (Rel (Record er))
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

-- | Restricts the relation given to only return unique records. Upshot
-- is all projected attributes will be 'grouped'.
unique :: Query ()
unique = Query (\(i, primQ) ->
    -- Add all non-aggregate expressions in the query
    -- to a groupby association list. This list holds the name
    -- of the expression and the expression itself. Those expressions
    -- will later by added to the groupby list in the SqlSelect built.
    case nonAggr primQ of
      [] -> ((), (i + 1, primQ)) -- No non-aggregate expressions - no-op.
      newCols -> ((), (i + 1, Group newCols primQ)))
  where
    -- Find all non-aggregate expressions and convert
    -- them to attribute expressions for use in group by.
    nonAggr :: PrimQuery -> Assoc
    nonAggr p = map toAttrExpr . filter (not . isAggregate . snd) . projected $ p 
    toAttrExpr (col, _) = (col, AttrExpr col)
    -- Find all projected columns from subqueries.
    projected :: PrimQuery -> Assoc
    projected (Project cols q) = cols
    projected (Restrict _ q) = projected q
    projected (Binary _ q1 q2) = projected q1 ++ projected q2
    projected (BaseTable tblName cols) = zip cols (map AttrExpr cols)
    projected (Special _ q) = projected q
    -- Group and Empty are no-ops
    projected (Group _ _) = []
    projected Empty  = []


-----------------------------------------------------------
-- Binary operations
-----------------------------------------------------------

binrel :: RelOp -> Query (Rel r) -> Query (Rel r) -> Query (Rel r)
binrel op (Query q1) (Query q2)
  = Query (\(i,primQ) ->
      let (Rel a1 scheme1,(j,primQ1)) = q1 (i,primQ)
          (Rel a2 scheme2,(alias,primQ2)) = q2 (j,primQ)
          
          scheme  = scheme1

          assoc1  = zip (map (fresh alias) scheme1)
          		(map (AttrExpr . fresh a1) scheme1)
          assoc2  = zip (map (fresh alias) scheme2)
          		(map (AttrExpr . fresh a2) scheme2)

          r1      = Project assoc1 primQ1
          r2      = Project assoc2 primQ2
          r       = Binary op r1 r2
      in
          (Rel alias scheme,(alias + 1, times r primQ)) )

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
table :: Table r -> Query (Rel r)
table (Table name assoc)
        = do
	  alias <- newAlias
          let newAssoc = map (\(attr,expr) -> (fresh alias attr,expr)) assoc
	      scheme   = map fst assoc
	      q        = Project newAssoc (BaseTable name scheme)
	  updatePrimQuery (times q)
          return (Rel alias scheme)

-- | Get the name of a table.
tableName :: Table t -> TableName
tableName (Table n _) = n

-- Type-level function to return the type of a table's row.
tableRec :: Table (Record r) -> Record r
tableRec = error "tableRec should never be evaluated."

-- | used in table definitions
baseTable :: (RecordLabels r ls, RecordValues r vs,
              HMapOut ShowLabelsOp ls String, HMapOut ToPrimExprsOp vs PrimExpr) =>
             TableName -> Table (Record r)
baseTable t   = let tbl = Table t (zip columns (map AttrExpr columns))
                    columns = labels $ tableRec tbl
                in tbl

-- | For queries against fake tables, such as
-- 'information_schema.information_schema_catalog_name'. Useful for
-- constructing queries that contain constant data (and do not select
-- from columns) but need a table to select from.
emptyTable :: TableName -> Table (Record HNil)
emptyTable t = Table t []

attribute :: String -> Expr a
attribute name  = Expr (AttrExpr name)

-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
-- | Create a named parameter with a default value.
namedParam :: Name -- ^ Name of the parameter.
  -> Expr a -- ^ Default value for the parameter.
  -> Expr a 
namedParam n (Expr def) = Expr (ParamExpr (Just n) def) 

-- | Create an anonymous parameter with a default value.
param :: Expr a -- ^ Default value.
  -> Expr a
param (Expr def) = Expr (ParamExpr Nothing def) 

unop :: UnOp -> Expr a -> Expr b
unop op (Expr primExpr)
                = Expr (UnExpr op primExpr)

binop :: BinOp -> Expr a  -> Expr b -> Expr c
binop op (Expr primExpr1) (Expr primExpr2)
                = Expr (BinExpr op primExpr1 primExpr2)

-- | Equality comparison on Exprs, = in SQL.
(.==.) :: Eq a => Expr a -> Expr a -> Expr Bool
(.==.) = binop OpEq

-- | Inequality on Exprs, <> in SQL.
(.<>.) :: Eq a => Expr a -> Expr a -> Expr Bool
(.<>.) = binop OpNotEq

(.<.) :: Ord a => Expr a -> Expr a -> Expr Bool
(.<.)  = binop OpLt

(.<=.) :: Ord a => Expr a -> Expr a -> Expr Bool
(.<=.) = binop OpLtEq

(.>.) :: Ord a => Expr a -> Expr a -> Expr Bool
(.>.)  = binop OpGt

(.>=.) :: Ord a => Expr a -> Expr a -> Expr Bool
(.>=.) = binop OpGtEq

-- | The inverse of an Expr Bool.
_not :: Expr Bool -> Expr Bool
_not   = unop OpNot

-- | \"Logical and\" on 'Expr', AND in SQL.
(.&&.):: Expr Bool -> Expr Bool -> Expr Bool
(.&&.) = binop OpAnd

-- | \"Logical or\" on 'Expr'. OR in SQL.
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
--
-- Note that SQL92 does not specify whether LIKE is case-sensitive or not.
-- Different database systems implement this differently.
like :: Expr String -> Expr String -> Expr Bool
like   = binop OpLike

-- | Returns true if the value of the first operand is
--   equal to the value of any of the expressions in the 
--   list operand. 
_in :: Eq a => Expr a -> [Expr a] -> Expr Bool
_in (Expr x) ys = Expr (BinExpr OpIn x (ListExpr [y | Expr y <- ys]))


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
mul :: Num a => Expr a -> Expr a -> Expr a
mul = numop OpMul
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

-- | Takes a default value a and a nullable value. If the value is NULL,
--   the default value is returned, otherwise the value itself is returned.
--   Simliar to 'fromMaybe'
fromNull :: Expr a         -- ^ Default value (to be returned for 'Nothing')
	 -> Expr (Maybe a) -- ^ A nullable expression
	 -> Expr a
fromNull d x@(Expr px) = _case [(isNull x, d)] (Expr px)

-- | Class which can convert BoundedStrings to normal strings,
-- even inside type constructors. Useful when a field
-- is defined as a BoundedString (e.g. "Expr BStr10" or "Expr (Maybe BStr20)") but
-- it needs to be used in an expression context. The example below illustrates a
-- table with at least two fields, strField and bStrField. The first is defined as
-- containing strings, the second as containing strings up to 10 characters long. The
-- @toStr@ function must be used to convert the bStrField into the appropriate type for
-- projecting as the strField:
--
-- > type SomeTable = (RecCons StrField (Expr String)
-- >                    (RecCons BStrField (Expr BStr10) ... ))
--
-- > someTable :: Table SomeTable
-- > someTable = ...
--
-- > strField :: Attr StrField String
-- > strField = ...
-- >
-- > bstrField :: Attr BStrField (BStr10)
-- > bstrField = ...
-- > 
-- > query = do
-- >  t <- table someTable
-- >  project $ strField << toStr $ t ! bstrField
--
class BStrToStr s d where
  -- | Convert a bounded string to a real string.
  toStr :: s -> d

instance (Size n) => BStrToStr (Expr (BoundedString n)) (Expr String) where
  toStr (Expr e) = (Expr e)

instance (Size n) => BStrToStr (Expr (Maybe (BoundedString n))) (Expr (Maybe String)) where
  toStr (Expr m) = (Expr m)

instance BStrToStr (Expr (Maybe String)) (Expr (Maybe String)) where
  toStr (Expr m) = (Expr m)

instance BStrToStr (Expr String) (Expr String) where
  toStr (Expr m) = (Expr m)


-----------------------------------------------------------
-- Using arbitrary SQL functions in a type-safe way.
-----------------------------------------------------------

-- | Used to implement variable length arguments to @func@, below.
class Args a where
  arg_ :: String -> [PrimExpr] -> a

-- | Used to limit variable argument form of @func@ to only take @Expr@ types,
-- and ignore @ExprAggr@ types.
class IsExpr a

instance (IsExpr tail) => IsExpr (Expr a -> tail)

instance IsExpr (Expr a)

instance (IsExpr tail, Args tail) => Args (Expr a -> tail) where
  arg_ name exprs = \(Expr prim) -> arg_ name (prim : exprs)

instance Args (Expr a) where
  -- Reverse necessary because arguments are built in reverse order by instances
  -- of Args above.
  arg_ name exprs = Expr (FunExpr name (reverse exprs))

instance Args (Expr a -> ExprAggr c) where
  arg_ name exprs = \(Expr prim) -> ExprAggr (AggrExpr (AggrOther name) prim)

{- | Can be used to define SQL functions which will
appear in queries. Each argument for the function is specified by its own Expr value. 
Examples include:

>  lower :: Expr a -> Expr (Maybe String) 
>  lower str = func "lower" str

The arguments to the function do not have to be Expr if they can
be converted to Expr:

>  data DatePart = Day | Century deriving Show 

>  datePart :: DatePart -> Expr (Maybe CalendarTime) -> Expr (Maybe Int) 
>  datePart date col = func "date_part" (constant $ show date) col

Aggregate functions can also be defined. For example:

 >  every :: Expr Bool -> ExprAggr Bool 
 >  every col = func "every" col

Aggregates are implemented to always take one argument, so any attempt to
define an aggregate with any more or less arguments will result in an error.

Note that type signatures are usually required for each function defined,
unless the arguments can be inferred.-}
func :: (Args a) => String -> a 
func name = arg_ name [] 

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
-- | Creates a constant expression from a haskell value.
constant :: ShowConstant a => a -> Expr a
constant x  = Expr (ConstExpr (showConstant x))

-- | Inserts the string literally - no escaping, no quoting.
literal :: String -> Expr a
literal x = Expr (ConstExpr (OtherLit x))

-- | Turn constant data into a nullable expression. 
--   Same as @constant . Just@
constJust :: ShowConstant a => a -> Expr (Maybe a)
constJust x = constant (Just x)

-- | Represents a null value.
constNull :: Expr (Maybe a)
constNull = Expr (ConstExpr NullLit)

-- | Generates a 'CAST' expression for the given
-- expression, using the argument given as the destination
-- type. 
cast :: String -- ^ Destination type.
  -> Expr a -- ^ Source expression.
  -> Expr b
cast typ (Expr expr) = Expr (CastExpr typ expr)

-- | Coerce the type of an expression
-- to another type. Does not affect the actual
-- primitive value - only the `phantom' type.
coerce :: Expr a -- ^ Source expression
  -> Expr b -- ^ Destination type.
coerce (Expr e) = Expr e

data ConstantRecordOp = ConstantRecordOp
instance ShowConstant v => Apply ConstantRecordOp (LVPair l v) (LVPair l (Expr v)) where
    apply _ (LVPair a) = LVPair (constant a)

constantRecord :: (HMap ConstantRecordOp r r') => Record r -> Record r'
constantRecord (Record r) = Record (hMap ConstantRecordOp r)

-----------------------------------------------------------
-- Aggregate operators
-----------------------------------------------------------

aggregate :: AggrOp -> Expr a -> ExprAggr b
aggregate op (Expr primExpr) = ExprAggr (AggrExpr op primExpr)

-- | Returns the number of records (=rows) in a query.
count :: Expr a -> ExprAggr Int
count x		= aggregate AggrCount x

-- | Returns the total sum of a column.
_sum :: Num a => Expr a -> ExprAggr a
_sum x = aggregate AggrSum x

-- | Returns the highest value of a column.
_max :: Ord a => Expr a -> ExprAggr a
_max x = aggregate AggrMax x

-- | Returns the lowest value of a column.
_min :: Ord a => Expr a -> ExprAggr a
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
top :: Int -> Query ()
top n = updatePrimQuery_ (Special (Top n))

-----------------------------------------------------------
-- Ordering results
-----------------------------------------------------------

orderOp :: (HasField f r (Expr e), ShowLabel f) => OrderOp -> Rel (Record r) -> f -> OrderExpr
orderOp op rel f = OrderExpr op expr
    where Expr expr = rel .!. f

-- | Use this together with the function 'order' to 
-- order the results of a query in ascending order.
-- Takes a relation and an attribute of that relation, which
-- is used for the ordering.
asc :: (HasField f r (Expr e), ShowLabel f) => Rel (Record r) -> f -> OrderExpr
asc rel f = orderOp OpAsc rel f

-- | Use this together with the function 'order' to 
-- order the results of a query in descending order.
-- Takes a relation and an attribute of that relation, which
-- is used for the ordering.
desc :: (HasField f r (Expr e), ShowLabel f) => Rel (Record r) -> f -> OrderExpr
desc rel f = orderOp OpDesc rel f

-- | Order the results of a query.
-- Use this with the 'asc' or 'desc' functions.
order :: [OrderExpr] -> Query ()
order xs = updatePrimQuery_ (Special (Order xs))

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

-- | Allows a subquery to be created between another query and
-- this query. Normally query definition is associative and query definition
-- is interleaved. This combinator ensures the given query is
-- added as a whole piece.
subQuery :: Query (Rel r) -> Query (Rel r)
subQuery (Query qs) = Query make
  where
    make (currentAlias, currentQry) =
        -- Take the query to add and run it first, using the current alias as
        -- a seed.
        let (Rel otherAlias otherScheme,(newestAlias, otherQuery)) = qs (currentAlias,Empty)
            -- Effectively renames all columns in otherQuery to make them unique in this
            -- query.
            assoc = zip (map (fresh newestAlias) otherScheme)
                        (map (AttrExpr . fresh otherAlias) otherScheme)
            -- Produce a query which is a cross product of the other query and the current query.
        in (Rel newestAlias otherScheme, (newestAlias + 1, times (Project assoc otherQuery) currentQry))
            
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

data ShowLabelsOp = ShowLabelsOp
instance ShowLabel x => Apply ShowLabelsOp x String where
    apply _ = showLabel

labels :: (HMapOut ShowLabelsOp ls String, RecordLabels r ls) => Record r -> [String]
labels r = hMapOut ShowLabelsOp (recordLabels r)

data ToPrimExprsOp = ToPrimExprsOp
instance ExprC e => Apply ToPrimExprsOp (e a) PrimExpr where
    apply _ = primExpr

exprs :: (RecordValues r vs, HMapOut ToPrimExprsOp vs PrimExpr) => Record r -> [PrimExpr]
exprs r = hMapOut ToPrimExprsOp (recordValues r)

data RecAttrOp a = RecAttrOp a
instance (HasField l (Rel t) v) => Apply (RecAttrOp (Rel t)) l (LVPair l v) where
    apply (RecAttrOp t) l = l .=. (t .!. l)
