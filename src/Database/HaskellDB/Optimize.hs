-----------------------------------------------------------
-- |
-- Module      :  Optimize
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  non portable
--
-- Defines standard optimizations performed on PrimQuery's
-- (relational expressions).
--
-----------------------------------------------------------

module Database.HaskellDB.Optimize (optimize) where

import Control.Exception (assert)
import Data.List (intersect)
import Database.HaskellDB.PrimQuery

-- | Optimize a PrimQuery
optimize :: PrimQuery -> PrimQuery
optimize = mergeProject
	   . removeEmpty
           . removeDead
           . pushRestrict

-- | Remove unused attributes from projections.
removeDead :: PrimQuery -> PrimQuery
removeDead query
        = removeD (attributes query) query

removeD :: Scheme -- ^ All live attributes (i.e. all attributes 
		  -- that are in the result of the query)
	-> PrimQuery
	-> PrimQuery
removeD live (Binary op query1 query2)
        = assert (all (`elem` (live1 ++ live2)) live)
		 Binary op (removeD live1 query1) (removeD live2 query2)
	  where
          live1 = live `intersect` attributes query1
	  live2 = live `intersect` attributes query2

removeD live (Project assoc query)
        = assert (all (`elem` (map fst newAssoc)) live)
          Project newAssoc (removeD newLive query)
        where
	  -- The live attributes in the nested query.
	  newLive :: Scheme
          newLive       = concat (map (attrInExpr . snd) newAssoc)

	  -- All associations that define attributes that are live
	  -- or that will be put in a GROUP BY clause.
	  -- These are the associations that will be kept.
	  newAssoc :: Assoc
          newAssoc      | hasAggregate  = groupAssoc ++ liveAssoc
          		| otherwise	= liveAssoc
          		where
          		  -- when an aggregate expression is in the
          		  -- association we check
          		  -- if an attribute is explicitly added by the user
			  -- and not already live
          		  -- (ie. "extend" is called), if so we should
          		  -- keep it live to be added in a GROUP BY
          		  -- clause.
          		  groupAssoc	    = filter (not.isLive)
          		  		    $ filter newAttr assoc
			  -- Does the association define a new attribute?
			  -- (i.e. not just pass on an existing one
			  -- from the nested query)
			  newAttr :: (Attribute,PrimExpr) -> Bool
          		  newAttr (attr,AttrExpr name)  = (attr /= name)
          		  newAttr _   		  	= True

          -- Is any live attribute bound to an aggregate expression?
	  hasAggregate :: Bool
	  hasAggregate  = any (isAggregate.snd) liveAssoc

	  -- All associations that define live attributes.
	  liveAssoc :: Assoc
          liveAssoc          = filter isLive assoc

	  -- Is the attribute defined by the association live?
	  isLive :: (Attribute,PrimExpr) -> Bool
          isLive (attr,expr) = attr `elem` live

removeD live (Restrict x query)
        = Restrict x (removeD (live ++ attrInExpr x) query)

removeD live (Special (Order xs) query)
	= Special (Order xs) (removeD (live ++ attrInOrder xs) query)

removeD live query
        = query


-- | Remove unused parts of the query
removeEmpty :: PrimQuery -> PrimQuery
removeEmpty
        = foldPrimQuery (Empty, BaseTable, project, restrict, binary, special)
        where
          -- Messes up queries without a table, e.g. constant queries
	  -- disabled by Bjorn Bringert 2004-04-08
          --project assoc Empty   = Empty
          project assoc query   | null assoc    = Empty
                                | otherwise     = Project assoc query

          restrict x Empty      = Empty
          restrict x query      = Restrict x query

          special op Empty	= Empty
          special op query	= Special op query

          binary op Empty query   = case op of Times      -> query
                                               _          -> Empty

          binary op query Empty   = case op of Times      -> query
                                               Difference -> query
                                               _          -> Empty
          binary op query1 query2 = Binary op query1 query2


-- | Collapse adjacent projections
mergeProject :: PrimQuery -> PrimQuery
mergeProject
        = foldPrimQuery (Empty,BaseTable,project,Restrict,Binary,Special)
        where
          project assoc1 (Project assoc2 query)
             	| safe newAssoc	  = Project newAssoc query
             	where
             	  newAssoc = subst assoc1 assoc2

	  -- "hmm, is this always true ?" (Daan Leijen)
	  -- "no, not true when assoc uses fields defined in only
	  -- one of assoc1 or assoc2" (Bjorn Bringert)
{-
          project assoc (Binary op (Project assoc1 query1)
          		           (Project assoc2 query2))
          	| safe newAssoc1 && safe newAssoc2
          		= Binary op (Project newAssoc1 query1)
          		            (Project newAssoc2 query2)
          		where
          		  newAssoc1  = subst assoc assoc1
          		  newAssoc2  = subst assoc assoc2
-}
          project assoc query
                = Project assoc query
	 
	  subst :: Assoc -- ^ Association that we want to change
		-> Assoc -- ^ Association containing the substitutions
		-> Assoc
          subst a1 a2
                = map (\(attr,expr) -> (attr, substAttr a2 expr)) a1

          safe :: Assoc -> Bool
          safe assoc
          	= not (any (nestedAggregate.snd) assoc)

-- | Push restrictions down through projections and binary ops.
pushRestrict :: PrimQuery -> PrimQuery

pushRestrict (Binary op query1 query2)
        = Binary op (pushRestrict query1) (pushRestrict query2)

pushRestrict (Project assoc query)
        = Project assoc (pushRestrict query)

-- restricts

pushRestrict (Restrict x (Project assoc query))
	     | safe = Project assoc (pushRestrict (Restrict expr query))
        where
	  -- since we passed a project, we need to replace all attributes
	  -- with the expression they are bound to by the project
          expr  = substAttr assoc x
	  -- aggregate expressions are not allowed in restricts
	  safe = not (isAggregate expr)

pushRestrict (Restrict x (Binary op query1 query2))
        | noneIn1   = Binary op query1 (pushRestrict (Restrict x query2))
        | noneIn2   = Binary op (pushRestrict (Restrict x query2)) query1
        -- otherwise fall through
        where
          attrs     = attrInExpr x
          noneIn1   = null (attrs `intersect` attributes query1)
          noneIn2   = null (attrs `intersect` attributes query2)

pushRestrict (Restrict x (query@(Restrict _ _)))
        = case (pushed) of
            (Restrict _ _)    -> Restrict x pushed
            _                 -> pushRestrict (Restrict x pushed)
        where
          pushed = pushRestrict query

pushRestrict (Restrict x (Special op query))
	= Special op (pushRestrict (Restrict x query))

pushRestrict (Restrict x query)
        = Restrict x (pushRestrict query)


-- also push specials

-- Order is only pushed if it does not cause it to
-- end up with aggregate expressions in one of its expressions
pushRestrict (Special (Order xs) (Project assoc query))
    | safe = Project assoc (pushRestrict (Special (Order xs') query))
	where
	  xs' = map (substAttr assoc) xs
	  safe = not (any isAggregate xs')

-- Top is always pushed through Project
pushRestrict (Special top@(Top _ _) (Project assoc query))
    = Project assoc (pushRestrict (Special top query))

pushRestrict (Special op (query@(Special _ _)))
	= case (pushed) of
	   (Special _ _)  -> Special op pushed
	   _		  -> pushRestrict (Special op pushed)
	where
          pushed = pushRestrict query

pushRestrict (Special op query)
	= Special op (pushRestrict query)

-- otherwise do nothing
pushRestrict query
        = query
