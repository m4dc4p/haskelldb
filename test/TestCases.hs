{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fcontext-stack100 #-}

module TestCases where

import qualified Control.Exception as E (Exception, throw, catch) -- for GHC > 6.10
import System.Time
import Test.HUnit
import Text.Regex
import Database.HaskellDB
import Database.HaskellDB.Query (tableName, constantRecord, subQuery, func, count, attributeName)
import Database.HaskellDB.HDBRec ((.=.))
import Control.Monad (when)
import Data.Typeable(Typeable)

import DB1
import DB1.String_tbl as TString
import DB1.Int_tbl as TInt
import DB1.Integer_tbl as TInteger
import DB1.Double_tbl as TDouble
import DB1.Bool_tbl as TBool
import DB1.Calendartime_tbl as TCalendartime
import DB1.Hdb_t1
import DBTest

tests :: Conn -> Test
tests = allTests hdb_test_db

allTests = 
    dbtests [
             tableTests,
             fieldTests,
             testDeleteAll,
             testDeleteNone,
             testUpdateNone,
             testTop,
             queryTests ,
             testOrder,
             testTransactionInsert,
             testInsertOnly insert string_tbl string_data_4
            ]

-- | Tests which cover generated SQL and do not require a database.
queryTests _ _ = TestList . map (\f -> f undefined undefined) $ [ testUnique1,
                       testUnique2,
                       testUnique3,
                       testUnique4,
                       testUnique5,
                       testUnique6,
                       testUnique7,
                       testUnique8,
                       testUnique9,
                       testUnique10,
                       testUnique11,
                       testAggr1,
                       testAggr2,
                       testAggrOrder,
                       testCorrectGroupBy,
                       testCorrectGroupByNoProjection,
                       testConcat,
                       testSubstring,
                       testFakeSelect,
                       testCopyAll,
                       testConcatAggr1,
                       testConcatAggr2,
                       testConcatAggr3
                      ]

tableTests = 
    dbtests [ 
             testTable string_tbl string_data_1,
             testTable int_tbl int_data_1,
             testTable integer_tbl integer_data_1,
             testTable double_tbl double_data_1,
             testTable bool_tbl bool_data_1,
             testTable calendartime_tbl calendartime_data_1
            ]

fieldTests = label "fieldTests" $
    dbtests [
             testField string_tbl string_data_1 TString.f01,
             testField string_tbl string_data_1 TString.f02,
             testField string_tbl string_data_1 TString.f03,
             testField string_tbl string_data_1 TString.f04,

             testField int_tbl int_data_1 TInt.f01,
             testField int_tbl int_data_1 TInt.f02,
             testField int_tbl int_data_1 TInt.f03,
             testField int_tbl int_data_1 TInt.f04,

             testField integer_tbl integer_data_1 TInteger.f01,
             testField integer_tbl integer_data_1 TInteger.f02,
             testField integer_tbl integer_data_1 TInteger.f03,
             testField integer_tbl integer_data_1 TInteger.f04,

             testField double_tbl double_data_1 TDouble.f01,
             testField double_tbl double_data_1 TDouble.f02,
             testField double_tbl double_data_1 TDouble.f03,
             testField double_tbl double_data_1 TDouble.f04,

             testField bool_tbl bool_data_1 TBool.f01,
             testField bool_tbl bool_data_1 TBool.f02,
             testField bool_tbl bool_data_1 TBool.f03,
             testField bool_tbl bool_data_1 TBool.f04,

             testField calendartime_tbl calendartime_data_1 TCalendartime.f01,
             testField calendartime_tbl calendartime_data_1 TCalendartime.f02,
             testField calendartime_tbl calendartime_data_1 TCalendartime.f03,
             testField calendartime_tbl calendartime_data_1 TCalendartime.f04 
            ]

strangeInputTests = label "strangeInputTests" $
    dbtests [
             testField string_tbl string_data_strange TString.f01,
             testField string_tbl string_data_strange TString.f02,
             testField string_tbl string_data_strange TString.f03,
             testField string_tbl string_data_strange TString.f04,

             testField int_tbl int_data_strange TInt.f01,
             testField int_tbl int_data_strange TInt.f02,
             testField int_tbl int_data_strange TInt.f03,
             testField int_tbl int_data_strange TInt.f04,

             testField integer_tbl integer_data_strange TInteger.f01,
             testField integer_tbl integer_data_strange TInteger.f02,
             testField integer_tbl integer_data_strange TInteger.f03,
             testField integer_tbl integer_data_strange TInteger.f04,

             testField double_tbl double_data_strange TDouble.f01,
             testField double_tbl double_data_strange TDouble.f02,
             testField double_tbl double_data_strange TDouble.f03,
             testField double_tbl double_data_strange TDouble.f04,

             testField calendartime_tbl calendartime_data_strange TCalendartime.f01,
             testField calendartime_tbl calendartime_data_strange TCalendartime.f02,
             testField calendartime_tbl calendartime_data_strange TCalendartime.f03,
             testField calendartime_tbl calendartime_data_strange TCalendartime.f04 
            ]

testTable tbl r = 
    dbtests [
             testUnique tbl r,
             testNonUnique tbl r
            ]

testField tbl r f = 
    dbtests [
             testInsertAndQuery tbl r f
            ]

testInsertAndQuery tbl r f = dbtest name $ \db ->
    do insert db tbl (constantRecord r)
       rs <- query db $ do t <- table tbl
                           project (f << t!f)
       assertEqual "Bad result length" 1 (length rs)
       assertSame "Bad field value" (r!f) (head rs!f) 
  where name = "insertAndQuery " ++ tableName tbl ++ "." ++ attributeName f


testUnique tbl r = dbtest name $ \db ->
    do insert db tbl (constantRecord r)
       insert db tbl (constantRecord r)
       rs <- query db $ do { t <- table tbl; unique; return t; }
       assertEqual "Bad result length" 1 (length rs)
  where name = "unique " ++ tableName tbl

testNonUnique tbl r = dbtest name $ \db ->
    do insert db tbl (constantRecord r)
       insert db tbl (constantRecord r)
       rs <- query db $ table tbl
       assertEqual "Bad result length" 2 (length rs)
  where name = "nonunique " ++ tableName tbl

testNotDistinct tbl r = dbtest name $ \db ->
    do insert db tbl (constantRecord r)
       insert db tbl (constantRecord r)
       rs <- query db $ table tbl
       assertEqual "Bad result length" 2 (length rs)
  where name = "not distinct " ++ tableName tbl

-- For running tests that don't really need a database connection, but
-- we want to include in the lists above
noDBTest name test = \_ _ -> TestLabel name (TestCase test)

-- Tests queries with aggregates and ORDER BY columns which do not appear
-- in SELECT still show up in GROUP BY. Note this
-- test does not require any DB access.
testAggrOrder = noDBTest "aggregate order by" $ do
    let qryTxt = showSql $ do
          t1 <- table int_tbl
          t2 <- table int_tbl
          order [asc t1 TInt.f02]
          project $ TInt.f02 << count(t1 ! TInt.f02) 
                      # TInt.f01 << (t2 ! TInt.f01) 
        -- Regex which ensures TInt.f02 column appears in GROUP BY, since it also appears
        -- in ORDER BY
        groupByTxt = mkRegex "GROUP BY.*\n.*f021.*\n.*ORDER BY.*f021"
        hasMatch = maybe (False) (const True) (matchRegex groupByTxt qryTxt)
    assertBool ("Expected columns did not appear in group by: " ++ qryTxt) hasMatch
  
-- Test that groupby clause is correctly tracked with projections
testCorrectGroupBy = noDBTest "Testing that groupby is correct with projections" $ do
  let qryTxt = showSql $ do
        p <- table int_tbl
        r <- project (TInt.f01 << (p ! TInt.f01))
        unique
        return r
      groupByTxt = mkRegex "GROUP BY.*f01.*"
      hasMatch = maybe (False) (const True) (matchRegex groupByTxt qryTxt)
  assertBool ("GROUP BY does not have correct columns: " ++ qryTxt) hasMatch

testCorrectGroupByNoProjection = noDBTest "Testing that groupby is correct without projection" $ do
  let qryTxt = showSql $ do
        p <- table int_tbl
        unique
        return p
      groupByTxt = mkRegex "GROUP BY.*f01,\n.*f02,\n.*f03,\n.*f04"
      hasMatch = maybe (False) (const True) (matchRegex groupByTxt qryTxt)
  assertBool ("GROUP BY does not have correct columns: " ++ qryTxt) hasMatch

testUnique1 = noDBTest "Testing that unique and count work together in a subquery." $ do
  let qryTxt = showSql $ do
        v <- subQuery $ do
              t1 <- table int_tbl
              unique
              project $ TInt.f02 << count(t1 ! TInt.f02)
        project $ TInt.f02 << (v ! TInt.f02)
      groupByTxt =  "SELECT COUNT(f021) as f02\n\
                    \FROM (SELECT f02 as f021\n\
                    \      FROM int_tbl as T1) as T1"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

testUnique2 = noDBTest "Testing that unique and subquery work together correctly." $ do
  let qryTxt = showSql $ do
        v <- subQuery $ do
              s <- table int_tbl
              restrict ((s ! TInt.f01) .==. constVal 100)
              unique
              project $ TInt.f02 << (s ! TInt.f02)
        project $ TInt.f02 << (v ! TInt.f02)
      groupByTxt =  "SELECT f02\n\
                    \FROM int_tbl as T1\n\
                    \WHERE f01 = 100\n\
                    \GROUP BY f02"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

testUnique3 = noDBTest "Testing that unique and restriction work correctly when an aggregate function is used at top level." $ do
  let qryTxt = showSql $ do
        v <- subQuery $ do
              s <- table int_tbl
              restrict ((s ! TInt.f01) .==. constVal 100)
              unique
              project $ TInt.f02 << (s ! TInt.f02)
        project $ TInt.f02 << count(v ! TInt.f02)
      groupByTxt =  "SELECT COUNT(f024) as f02\n\
                    \FROM (SELECT f02 as f024\n\
                    \      FROM int_tbl as T1\n\
                    \      WHERE f01 = 100\n\
                    \      GROUP BY f02) as T1"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

testUnique4 = noDBTest "Testing that unique in top-level query works." $ do
  let qryTxt = showSql $ do
        s <- table int_tbl
        restrict ((s ! TInt.f01) .==. constVal 100)
        unique
        project $ TInt.f02 << (s ! TInt.f02)
      groupByTxt = "SELECT f02\n\
                   \FROM int_tbl as T1\n\
                   \WHERE f01 = 100\n\
                   \GROUP BY f02"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

testUnique5 = noDBTest "Testing that unique, restrict and count in subquery works." $ do
  let qryTxt = showSql $ do
        v <- subQuery $ do
          s <- table int_tbl
          restrict ((s ! TInt.f01) .==. constVal 100)
          unique
          project $ TInt.f02 << count(s ! TInt.f02)
        project $ TInt.f02 << (v ! TInt.f02) 
      groupByTxt =  "SELECT COUNT(f021) as f02\n\
                    \FROM (SELECT f02 as f021\n\
                    \      FROM int_tbl as T1\n\
                    \      WHERE f01 = 100) as T1"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

testUnique6 = noDBTest "Testing that unique in subquery and restriction at top-level works." $ do
  let qryTxt = showSql $ do
        v <- subQuery $ do
          s <- table int_tbl
          unique
          project $ TInt.f01 << (s ! TInt.f03)
        restrict ((v ! TInt.f01) .==. constVal 100)
        project $ TInt.f01 << (v ! TInt.f01)
      groupByTxt =  "SELECT f013 as f01\n\
                    \FROM (SELECT f03 as f013\n\
                    \      FROM int_tbl as T1\n\
                    \      GROUP BY f03) as T1\n\
                    \WHERE f013 = 100"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

testUnique7 = noDBTest "Testing that unique in subquery and restriction plus count at top-level works." $ do
  let qryTxt = showSql $ do
                 v <- subQuery $ do
                           s <- table int_tbl
                           unique
                           project $ TInt.f02 << (s ! TInt.f04)
                 restrict $ (v ! TInt.f02) .==. constant 100
                 project $ TInt.f02 << count(v ! TInt.f02)
      groupByTxt =  "SELECT COUNT(f023) as f02\n\
                    \FROM (SELECT f04 as f023\n\
                    \      FROM int_tbl as T1\n\
                    \      GROUP BY f04) as T1\n\
                    \WHERE f023 = 100"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

testUnique8 = noDBTest "Testing that group by works correctly with projected expressions (instead of just columns)" $ do
  let qryTxt = showSql $ do
                 v <- subQuery $ do
                           h <- table int_tbl
                           project $ TInt.f02 << _case [((h ! TInt.f02) .==. constant 100, constant 0)] (constant (1::Int)) 
                 unique
                 return v
      groupByTxt = "SELECT CASE WHEN f02 = 100 THEN 0 ELSE 1 END as f02\n\
                   \FROM int_tbl as T1\n\
                   \GROUP BY (CASE WHEN f02 = 100 THEN 0 ELSE 1 END)"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

testUnique9 = noDBTest "Testing that group by works correctly with projected expressions and aggregates" $ do
  let qryTxt = showSql $ do
                 v <- subQuery $ do
                           h <- table int_tbl
                           project $ TInt.f02 << _case [((h ! TInt.f02) .==. constant 100, constant 0)] (constant (1::Int)) 
                                     # TInt.f04 << count (h ! TInt.f01)
                 unique
                 project $ copyAll v
      groupByTxt = "SELECT CASE WHEN f02 = 100 THEN 0 ELSE 1 END as f02,\n\
                   \       COUNT(f01) as f04\n\
                   \FROM int_tbl as T1\n\
                   \GROUP BY (CASE WHEN f02 = 100 THEN 0 ELSE 1 END)"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

testUnique10 = noDBTest "Testing that count works when  unique is in a subquery." $ do
  let qryTxt = showSql $ do
        v <- subQuery $ do
              t1 <- table int_tbl
              unique
              project $ TInt.f02 << (t1 ! TInt.f02)
        project $ TInt.f02 << count(v ! TInt.f02)
      groupByTxt =  "SELECT COUNT(f024) as f02\n\
                    \FROM (SELECT f02 as f024\n\
                    \      FROM int_tbl as T1\n\
                    \      GROUP BY f02) as T1"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

testUnique11 = noDBTest "Testing that count works when unique and restrict in a subquery." $ do
  let qryTxt = showSql $ do
        v <- subQuery $ do
          s <- table int_tbl
          restrict ((s ! TInt.f01) .==. constVal 100)
          unique
          project $ TInt.f02 << (s ! TInt.f02) 
        project $ TInt.f02 << count(v ! TInt.f02)
      groupByTxt = "SELECT COUNT(f024) as f02\n\
                   \FROM (SELECT f02 as f024\n\
                   \      FROM int_tbl as T1\n\
                   \      WHERE f01 = 100\n\
                   \      GROUP BY f02) as T1"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

-- This query will fail to compile if instances from Data.HList.TypeCastGeneric1 are
-- not imported.
testHasField = noDBTest "Ensuring HasField works with restrict." $ do
  let qryTxt = do
        s <- table int_tbl
        restrict ((s ! TInt.f02) .==. (s ! TInt.f02))
        project $ TInt.f03 << (s ! TInt.f03)
      groupByTxt =  "";
  assertBool "If this test compiles it worked." True

-- This test ensures that 'fake' results can be built in code. An 
-- incorrect optimization was removing projections w/ no columns, which
-- resulted in the table associated with a query being lost.
testFakeSelect = noDBTest "Testing that fake tables can be built in code." $ do
  let qryTxt = showSql $ do {
        h <- table int_tbl;
        project $ TInt.f02 << constant (1 :: Int); }
      expected =  "SELECT 1 as f02\n\
                   \FROM int_tbl as T1"
  assertQueryText "Did not generate expected query. " qryTxt expected

testAggr1 = noDBTest "Testing that group does use projected expressions when aggregate is present" $ do
  let qryTxt = showSql $ do
        h <- table int_tbl
        project $ TInt.f02 << _case [((h ! TInt.f02) .==. constant 100, constant 0)] (constant (1::Int)) 
                    # TInt.f04 << count (h ! TInt.f01)
      groupByTxt = "SELECT CASE WHEN f02 = 100 THEN 0 ELSE 1 END as f02,\n\
                   \       COUNT(f01) as f04\n\
                   \FROM int_tbl as T1\n\
                   \GROUP BY (CASE WHEN f02 = 100 THEN 0 ELSE 1 END)"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt

testConcat = noDBTest "Testing SQL concat query" $ do
  let qryTxt = showSql $ do
        h <- table string_tbl
        project $ TString.f02 << concatF (h ! TString.f02) (h ! TString.f04)  
      result = "SELECT concat(f02,f04) as f02\n\
                \FROM string_tbl as T1"
  assertQueryText "Concat not generated as expected: " qryTxt result

concatF :: Expr String -> Expr String -> Expr String
concatF str1 str2 = func "concat" str1 str2

substringF :: Expr String -> Expr Int -> Expr Int -> Expr String
substringF str idx len = func "substring" str idx len

testSubstring = noDBTest "Testing SQL concat query" $ do
  let qryTxt = showSql $ do
        h <- table string_tbl
        project $ TString.f02 << substringF (h ! TString.f02) (constant 0) (constant 5) 
      result = "SELECT substring(f02,0,5) as f02\n\
                \FROM string_tbl as T1"
  assertQueryText "Substring not generated as expected: " qryTxt result

{-| This query caused an exception due to a bug in defaultSqlBinary. Ensure that doesn't
    happen again. Additionally, this test makes sure base tables aren't
    dropped, even if they have no columns used. A cross-join may be desired for
    some reason. Otherwise, why include the table?
-}
testCopyAll = noDBTest "Test copyAll operator" $ do 
  let qryTxt = showSql $ do
        t1 <- table int_tbl
        t2 <- table string_tbl
        project $ copyAll t2 
      expected = "SELECT f012 as f01,\n\
                 \       f022 as f02,\n\
                 \       f032 as f03,\n\
                 \       f042 as f04\n\
                 \FROM (SELECT f01 as f012,\n\
                 \             f02 as f022,\n\
                 \             f03 as f032,\n\
                 \             f04 as f042\n\
                 \      FROM string_tbl as T1) as T1,\n\
                 \     int_tbl as T2"
  assertQueryText "Test that copyAll generates query correctly, even if not all columns are used." qryTxt expected

testAggr2 = noDBTest "Testing that top level constants don't cause an extra groupby" $ do
  let qryTxt = showSql $ do 
                 v <- subQuery $ do 
                           h <- table int_tbl
                           project $ TInt.f02 <<- 100
                                     # TInt.f04 << count (h ! TInt.f01)
                 unique
                 project $ copyAll v
      groupByTxt = "SELECT 100 as f02,\n\
                   \       COUNT(f01) as f04\n\
                   \FROM int_tbl as T1"
  assertQueryText "Did not generate expected query. " qryTxt groupByTxt


-- | Test non-aggregate functions with aggregates.
testConcatAggr1 = noDBTest "Testing SQL concat query with unique." $ do
  let qryTxt = showSql $ do
        h <- table string_tbl
        unique
        project $ TString.f02 << concatF (h ! TString.f02) (h ! TString.f04)  
      result = "SELECT concat(f021,f041) as f02\n\
               \FROM (SELECT f02 as f021,\n\
               \             f04 as f041\n\
               \      FROM string_tbl as T1) as T1\n\
               \GROUP BY (concat(f021,f041))"
  assertQueryText "Concat not generated as expected: " qryTxt result


testConcatAggr2 = noDBTest "Testing SQL concat query with a count" $ do
  let qryTxt = showSql $ do
                 v <- subQuery $ do
                           h <- table string_tbl
                           unique
                           project $ TString.f02 << concatF (h ! TString.f02) (h ! TString.f04)
                 project $ TInt.f02 << count (v ! TString.f02)
      result = "SELECT COUNT(f024) as f02\n\
               \FROM (SELECT concat(f021,f041) as f024\n\
               \      FROM (SELECT f02 as f021,\n\
               \                   f04 as f041\n\
               \            FROM string_tbl as T1) as T1\n\
               \      GROUP BY (concat(f021,f041))) as T1"
  assertQueryText "Concat not generated as expected: " qryTxt result

testConcatAggr3 = noDBTest "Testing SQL concat query with a count and unique." $ do
  let qryTxt = showSql $ do
                 v <- subQuery $ do
                           h <- table string_tbl
                           project $ TString.f02 << concatF (h ! TString.f02) (h ! TString.f04)
                 project $ TInt.f02 << count (v ! TString.f02)
      result = "SELECT COUNT(concat(f02,f04)) as f02\n\
               \FROM string_tbl as T1"
  assertQueryText "Concat not generated as expected: " qryTxt result


-- | Helper which asserts that two query strings are equal.
assertQueryText msg query expect = assertBool (msg ++ "\nGot: \n\n" ++ query ++
                                               "\n\nand expected: \n\n" ++ expect)
                                              (query == expect)

-- * Insert

testInsert = dbtest "insert" $ \db ->
    do insertData db hdb_t1 hdb_t1_data
       rs <- query db $ table hdb_t1
       assertEqual "Bad result length" (length hdb_t1_data) (length rs)

testInsertOnly ins tbl r = dbtest name $ \db ->
  ins db tbl (constantRecord r)
  where name = "rearrange " ++ tableName tbl


-- * Delete

testDeleteAll = dbtest "deleteAll" $ \db ->
    do insertData db hdb_t1 hdb_t1_data
       delete db hdb_t1 (\_ -> constant True)
       rs <- query db $ table hdb_t1
       assertBool "Query after complete delete is non-empty" (null rs)

testDeleteNone = dbtest "deleteNone" $ \db ->
    do insertData db hdb_t1 hdb_t1_data
       rs <- query db $ table hdb_t1
       delete db hdb_t1 (\_ -> constant False)
       rs' <- query db $ table hdb_t1
       assertEqual "Something was changed by a null delete" rs rs'

-- * Update

testUpdateNone = dbtest "updateNone" $ \db ->
    do insertData db hdb_t1 hdb_t1_data
       rs <- query db $ table hdb_t1
       update db hdb_t1 (\_ -> constant False) (\_ -> (t1f02 << constant "flubber"))
       rs' <- query db $ table hdb_t1
       assertEqual "Something was changed by a null update" rs rs'


testTop = dbtest "top" $ \db ->
    do insertData db hdb_t1 hdb_t1_data
       rs <- query db $ do t <- table hdb_t1
                           top 1
                           return t
       assertEqual "Result count" 1 (length rs)

testOrder = dbtest "order" $ \db ->
    do insert db string_tbl (constantRecord string_data_1)
       insert db string_tbl (constantRecord string_data_2)
       rs <- query db $ do t <- table string_tbl
                           order [asc t TString.f01]
                           return t
       assertEqual "Result count" 2 (length rs)
       assertEqual "First record" string_data_2 (rs !! 0)
       assertEqual "Second record" string_data_1 (rs !! 1)

testTransactionInsert dbi conn = TestCase $ E.catch (realTest >> assertFailure "Foo") doTest
    where 
      doTest :: AbortTransaction -> IO ()
      doTest _ = return () -- withDB checkForTable conn
      (TestLabel _ (TestCase realTest)) = dbtest "transactionInsert" test dbi conn
      test db = transaction db $ do 
                  insertData db hdb_t1 hdb_t1_data
                  E.throw AbortTransaction
                  return ()
      checkForTable db = do 
        ts <- tables db
        when (tableName hdb_t1 `elem` ts) 
             (assertFailure $ tableName hdb_t1 ++ " should not exist after transaction failure.")


data AbortTransaction = AbortTransaction
  deriving (Show, Typeable)

instance E.Exception AbortTransaction

-- * Utilities

assertTableEmpty db tbl =
    do rs <- query db $ table tbl
       assertBool "Table not empty" (null rs)

assertSame :: (Show a, Same a) => String -> a -> a -> Assertion
assertSame s x y = assertBool msg (same x y) 
        where msg = s ++ " Expected: " ++ show x ++ ", got " ++ show y

insertData db tbl = mapM_ (insert db tbl)

sameClockTime :: CalendarTime -> CalendarTime -> Bool
sameClockTime t1 t2 = toClockTime t1 == toClockTime t2

-- Hack to replace Eq CalendarTime
class Eq a => Same a where
    same :: a -> a -> Bool
    same = (==)

instance Same a => Same [a] where
    same [] [] = True
    same (x:xs) (y:ys) = same x y && same xs ys
    same _ _ = False

instance Same a => Same (Maybe a) where
    same Nothing Nothing = True
    same (Just x) (Just y) = same x y
    same _ _ = False

instance Same Char
instance Same Int
instance Same Integer
instance Same Double
instance Same Bool
instance Same CalendarTime where
    same = sameClockTime

-- * Test data

string_data = [string_data_1,string_data_2,string_data_3]

string_data_1 =
          TString.f01 .=. Just "foo" #
          TString.f02 .=. "bar" #
          TString.f03 .=. Nothing #
          TString.f04 .=. "baz" 

string_data_2 =
          TString.f01 .=. Just "asdas" #
          TString.f02 .=. "dast fsdf e" #
          TString.f03 .=. Nothing #
          TString.f04 .=. "jhasiude94" 

string_data_3 =
          TString.f01 .=. Just "dafjht" #
          TString.f02 .=. "adsfkasdjfklsadjfalsdf" #
          TString.f03 .=. Nothing #
          TString.f04 .=. "xxxxxxxx" 

-- Test for field rearrangment inside insert,
-- and for the type of F03 not being fixed.
string_data_4 =
          TString.f01 .=. Just "dafjht" #
          TString.f02 .=. "adsfkasdjfklsadjfalsdf" #
          TString.f03 .=. Nothing #
          TString.f04 .=. "xxxxxxxx" 

-- Test for defaulting of Maybe columns -- F03 should default to Nothing.
string_data_5 =
          TString.f01 .=. Just "dafjht" #
          TString.f02 .=. "adsfkasdjfklsadjfalsdf" #
          TString.f04 .=. "xxxxxxxx" 

string_data_strange = 
          TString.f01 .=. Just "'\"\\;" #
          TString.f02 .=. "\n\r\t " #
          TString.f03 .=. Nothing #
          TString.f04 .=. "\255\246\0" 

int_data_1 = 
          TInt.f01 .=. Just 42 #
          TInt.f02 .=. 43 #
          TInt.f03 .=. Nothing #
          TInt.f04 .=. (-1234) 

int_data_strange = 
          TInt.f01 .=. Just 2147483647 #
          TInt.f02 .=. (-2147483648) #
          TInt.f03 .=. Nothing #
          TInt.f04 .=. 0 

integer_data_1 = 
          TInteger.f01 .=. Just 1 #
          TInteger.f02 .=. 123 #
          TInteger.f03 .=. Nothing #
          TInteger.f04 .=. (-453453) 

integer_data_strange = 
          TInteger.f01 .=. Just 1234567890123456789012345678901234567890 #
          TInteger.f02 .=. (-35478572384578913475813465) #
          TInteger.f03 .=. Nothing #
          TInteger.f04 .=. (-1) 

double_data_1 = 
          TDouble.f01 .=. Just 0.0 #
          TDouble.f02 .=. 4.245 #
          TDouble.f03 .=. Nothing #
          TDouble.f04 .=. (-8.6e15) 

double_data_strange = 
          TDouble.f01 .=. Just (-0.0) #
          TDouble.f02 .=. pi #
          TDouble.f03 .=. Nothing #
          TDouble.f04 .=. (-8.6e37) 

bool_data_1 = 
          TBool.f01 .=. Just True #
          TBool.f02 .=. True  #
          TBool.f03 .=. Nothing #
          TBool.f04 .=. False 

calendartime_data_1 = 
          TCalendartime.f01 .=. Just epoch #
          TCalendartime.f02 .=. epoch #
          TCalendartime.f03 .=. Nothing #
          TCalendartime.f04 .=. someTime 

calendartime_data_strange = 
          TCalendartime.f01 .=. Just (epoch { ctYear = 1969 }) #
          TCalendartime.f02 .=. someTime { ctYear = 2040 } #
          TCalendartime.f03 .=. Nothing #
          TCalendartime.f04 .=. epoch { ctYear = 1000 } 

hdb_t1_data = [hdb_t1_data_1] 

hdb_t1_data_1 = 
          t1f01 <<- Just "foo" #
          t1f02 <<- "bar" #
          t1f03 <<- Nothing #
          t1f04 <<- "baz" #

          t1f05 <<- Just 42 #
          t1f06 <<- 43 #
          t1f07 <<- Nothing #
          t1f08 <<- (-1234) #

          t1f09 <<- Just 324234 #
          t1f10 <<- 123 #
          t1f11 <<- Nothing #
          t1f12 <<- (-453453) #

          t1f13 <<- Just 0.0 #
          t1f14 <<- pi #
          t1f15 <<- Nothing #
          t1f16 <<- (-8.6e15) #

-- Disabled for now, since booleans don't really work anywhere
--          t1f17 << Just True #
--          t1f18 << True  #
--          t1f19 << Nothing #
--          t1f20 << False #

          t1f21 <<- Just epoch #
          t1f22 <<- epoch #
          t1f23 <<- Nothing #
          t1f24 <<- someTime 





epoch = CalendarTime {
                      ctYear = 1970,
                      ctMonth = January,
                      ctDay = 1,
                      ctHour = 0,
                      ctMin = 0,
                      ctSec = 0,
                      ctPicosec = 0,
                      ctWDay = Thursday,
                      ctYDay = 0,
                      ctTZName = "UTC",
                      ctTZ = 0,
                      ctIsDST = False
                     }

someTime = CalendarTime {
                         ctYear = 2006, 
                         ctMonth = July, 
                         ctDay = 18, 
                         ctHour = 13, 
                         ctMin = 37, 
                         ctSec = 15, 
                         ctPicosec = 0, 
                         ctWDay = Tuesday, 
                         ctYDay = 198, 
                         ctTZName = "PDT", 
                         ctTZ = -25200, 
                         ctIsDST = True
                        }
