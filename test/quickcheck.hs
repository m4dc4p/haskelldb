import Debug.QuickCheck

import Database.HaskellDB
import Database.HaskellDB.Database

import Dp037.D3proj_time_reports

import TestConnect

{-

Possible properties:

-- optimization does not change result

-- lazy and strict give same results

-- inserting a row and then retrieving it gives the original values

-- show . read == id for records

-- read . show == id for records

-- read . show == id for records

-- creating a table and describing it gives the orginal spec

-- same operations on different databases gie the same result

-}

-- FIXME: allow row permutations?
resultEq :: Eq r => [Row r] -> [Row r] -> Bool
resultEq [] [] = True
resultEq (Row x:xs) (Row y:ys) = x == y
resultEq _ _ = False

sameResults :: (GetRec r vr, Eq vr) => 
	       Database -> Query (Rel r) -> Query (Rel r) -> IO Bool
sameResults db q1 q2 = do
		       rs1 <- query db q1
		       rs2 <- query db q2
		       return $ resultEq rs1 rs2

q1 = do
     r <- table d3proj_time_reports
     restrict (r!userid .==. constant "d00bring")
     restrict (r!hours .>. constant 0.5)
     return r

q2 = do
     r <- table d3proj_time_reports
     restrict (r!userid .==. constant "d00bring" 
	       .&&. r!hours .>. constant 0.5)
     return r

t db = do
       sameResults db q1 q2 >>= putStrLn . show

main = argConnect t