-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.FlatDB
-- Copyright   :  Bjorn Bringert 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
--  This is a very experimental HaskellDB back-end which is written in pure Haskell
--  and doesn't use SQL. It stores the database in a file. Using this with
--  concurrent writes leads to data loss. This back-end does not support transactions.
-----------------------------------------------------------
module Database.HaskellDB.FlatDB (DriverInterface(..), driver, 
                                  withFlatDB, newDB) where

import Database.HaskellDB.Database
import Database.HaskellDB.FieldType
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query	hiding (isNull, union, intersect)
import Database.HaskellDB.DriverAPI
import Database.HaskellDB (Record)

import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Error ()
import Control.Monad.Trans
import Data.Bits
import Data.Function (on)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (mconcat)
import Data.List
import Data.Maybe
import Prelude hiding (toInteger)
import System.Time
import System.Directory
import System.IO

import Debug.Trace

type FlatDB = Map TableName FlatTable
type RelSchema = [(Attribute,FieldDesc)]
type FlatTable = FlatRel

data FlatRel = FlatRel { relSchema :: RelSchema,
                         relRows :: [FlatRow] }
             deriving (Show,Read)

type FlatRow = [(String,Value)]

data Value = VString String
           | VInteger Integer
           | VDouble Double
           | VBool Bool
           | VDate CalendarTime
           | VNull
             deriving (Show,Read,Eq,Ord) -- FIXME: ord and eq are too liberal here?


-- | This driver requires the following options: 
--   "filepath"
driver :: DriverInterface
driver = defaultdriver {connect = flatDBConnectOpts}


flatDBConnectOpts :: MonadIO m => [(String,String)] -> (Database -> m a) -> m a
flatDBConnectOpts opts f = do [a] <- getOptions ["filepath"] opts
                              withFlatDB a f

-- FIXME: this is full of race conditions
withFlatDB :: MonadIO m => FilePath -> (Database -> m a) -> m a
withFlatDB f m = 
    do e <- liftIO $ doesFileExist f
       when (not e) $ liftIO $ newDB f
       db <- liftIO $ fileReadDB f
       dbr <- liftIO $ newIORef db
       x <- m (flatDatabase dbr)
       db' <- liftIO $ readIORef dbr
       (tmpFile,tmpH) <- liftIO $ openTempFile "." (f++".tmp")
       liftIO $ hWriteDB tmpH db'
       liftIO $ hClose tmpH
       liftIO $ renameFile tmpFile f
       return x

emptyDB :: FlatDB
emptyDB = Map.empty

modifyDB :: IORef FlatDB -> (FlatDB -> FlatDB) -> IO ()
modifyDB db f = modifyIORef db f

queryDB :: IORef FlatDB -> (FlatDB -> a) -> IO a
queryDB db f = liftM f $ readIORef db

flatDatabase :: IORef FlatDB -> Database
flatDatabase db = 
    Database { dbQuery        = \x y -> readIORef db >>= \d -> flatQueryHDB x y d,
    	       dbInsert       = \x y -> modifyDB db $ flatInsert x y,
	       dbInsertQuery  = \x y -> modifyDB db $ flatInsertQuery x y,
	       dbDelete	      = \x y -> modifyDB db $ flatDelete x y,
	       dbUpdate	      = \x y z -> modifyDB db $ flatUpdate x y z,
	       dbTables       = queryDB db $ flatTables,
	       dbDescribe     = \x -> queryDB db $ flatDescribe x,
	       dbTransaction  = notImplemented "dbTransaction",
	       dbCreateDB     = \x -> return (),
	       dbCreateTable  = \x y -> modifyDB db $ flatCreateTable x y,
	       dbDropDB       = notImplemented "dbDropDB",
	       dbDropTable    = \x -> modifyDB db $ flatDropTable x
	     }

fileReadDB :: FilePath -> IO FlatDB
fileReadDB f = openFile f ReadMode >>= hReadDB

hReadDB :: Handle -> IO FlatDB
hReadDB h = 
    do c <- hGetContents h
       case readDB c of
           Left err ->
               fail $ "FlatDB error when reading " ++ show h
                        ++ ": " ++ err
           Right db -> return db

fileWriteDB :: FilePath -> FlatDB -> IO ()
fileWriteDB f db = bracket (openFile f WriteMode) hClose (flip hWriteDB db) 

hWriteDB :: Handle -> FlatDB -> IO ()
hWriteDB h db = hPutStr h $ showDB db

newDB :: FilePath -> IO ()
newDB f = fileWriteDB f emptyDB

showDB :: FlatDB -> String
showDB = show . Map.toList 

readDB :: Monad m => String -> m FlatDB
readDB c = case reads c of
                []       -> fail "parse error"
                [(x,[])] -> return $ Map.fromList x
                [(_,_)]  -> fail "junk at end"
                _        -> fail "ambiguous parse"

-- Relations

-- FIXME: make distinct
relInsert :: FlatRow -> FlatRel -> FlatRel
relInsert r t = t { relRows = r : relRows t }

-- FIXME: make distinct
-- FIXME: assert relSchema t1 == relSchema t2
relUnion :: FlatRel -> FlatRel -> FlatRel
relUnion t1 t2 = t1 { relRows = relRows t1 `union` relRows t2 }  

relFilter :: (FlatRow -> Bool) -> FlatRel -> FlatRel
relFilter p t = t { relRows = filter p (relRows t) }  

-- FIXME: make distinct
relMap :: (FlatRow -> FlatRow) -> FlatRel -> FlatRel
relMap f t = t { relRows = map f (relRows t) }



modifyTable :: TableName -> (FlatRel -> FlatRel) -> FlatDB -> FlatDB
modifyTable n f = Map.adjust f n -- FIXME: does nothing if the table doesn't exist

flatTrace :: String -> a -> a
--flatTrace = trace -- ENABLE FOR TRACING
flatTrace _ x = x

flatQueryHDB :: GetRec er vr => PrimQuery -> Rel er -> FlatDB -> IO [Record vr]
flatQueryHDB q rel db = 
       flatTrace (show q) $
       mapM (getRec flatGetInstances rel scheme) rs
    where scheme = attributes q
          rs = relRows $ flatQuery q db


flatGetInstances :: GetInstances FlatRow
flatGetInstances = 
    GetInstances {
		  getString        = flatGetFieldM toString
		 , getInt          = flatGetFieldM toInt
		 , getInteger      = flatGetFieldM toInteger
		 , getDouble       = flatGetFieldM toDouble
		 , getBool         = flatGetFieldM toBool
		 , getCalendarTime = flatGetFieldM toDate
		 }
  where flatGetFieldM f r l = return $ flatGetField f r l




flatQuery :: PrimQuery -> FlatDB -> FlatRel
flatQuery pq db = e pq
 where 
   e q = case q of
           BaseTable n _ -> case Map.lookup n db of
                              Just t -> t
                              Nothing -> error $ "Table not found: " ++ n
           Project bs x ->
               FlatRel { relSchema = [(n, inferType s e) | (n,e) <- (gbs++abs)], -- FIXME: what about nullability?
                         relRows = rps3 }
               where FlatRel { relSchema = s, relRows = rs } = e x
                     (abs,gbs) = partition (isAggregate . snd) bs
                     rps1 = map (\r -> (evalBinds r gbs, r)) rs
                     rps2 = groupBy (\ (x1,_) (x2,_) -> x1 == x2) rps1 -- FIXME: BUG: what if groups are not contiguous?
                     rps3 | null gbs && null rs = [[(n,evalAggr [] e) | (n,e) <- abs]] -- return one result if there are no non-aggregates and the relation is empty
                          | otherwise = map (\g -> fst (head g) ++ [(n,evalAggr (map snd g) e) | (n,e) <- abs]) rps2
           Restrict p x -> relFilter (\r -> toBool (evalExpr r p)) t
               where t = e x
           Group gs x -> FlatRel { relSchema = s, relRows = rs' }
               where FlatRel { relSchema = s, relRows = rs } = e x
                     -- FIXME: BUG: what if groups are not contiguous?
                     rs' = map (fst . head) $ groupBy ((==) `on` snd) [(r,evalBinds r gs) | r <- rs]
           Binary op x1 x2 ->
               case op of
                   Times -> FlatRel { relSchema = s1 ++ s2, -- FIXME: assert s1, s2 disjoint
                                      relRows = [r1 ++ r2 | r1 <- rs1, r2 <- rs2] }
                   Union -> FlatRel { relSchema = s1, -- FIXME: assert t1 == t2
                                      relRows = rs1 `union` rs2 }
                   Intersect -> FlatRel { relSchema = s1, -- FIXME: assert t1 == t2
                                          relRows = rs1 `intersect` rs2 }
                   Divide -> notImplemented $ show op -- FIXME: hairy, do it later
                   Difference -> FlatRel { relSchema = s1, -- FIXME: assert t1 == t2
                                           relRows = rs1 \\ rs2 }
             where 
               FlatRel { relSchema = s1, relRows = rs1 } = e x1
               FlatRel { relSchema = s2, relRows = rs2 } = e x2
           Special op q ->
               case op of 
                 Order os -> t { relRows = sortBy sortExprs (relRows t) }
                       where sortExprs r1 r2 = mconcat [cmpExpr o e r1 r2 | OrderExpr o e <- os]
                             cmpExpr OpAsc  e r1 r2 = evalExpr r1 e `compare` evalExpr r2 e
                             cmpExpr OpDesc e r1 r2 = evalExpr r2 e `compare` evalExpr r1 e
                 Top n -> t { relRows = take n (relRows t) }
               where t = e q
           Empty -> FlatRel { relSchema = [], 
                              relRows = [] }

flatInsert :: TableName -> Assoc -> FlatDB -> FlatDB
flatInsert n xs = flatTrace ("Insert: " ++ show xs) $ 
                  modifyTable n (relInsert r)
    where r = evalBinds [] xs

flatInsertQuery :: TableName -> PrimQuery -> FlatDB -> FlatDB
flatInsertQuery n q db = modifyTable n (relUnion rs) db
    where rs = flatQuery q db

flatDelete :: TableName -> [PrimExpr] -> FlatDB -> FlatDB
flatDelete n cs = flatTrace ("Delete: " ++ n ++ ", " ++ show cs) $ 
                  modifyTable n (relFilter (not . p))
    where p r = all (\c -> toBool (evalExpr r c)) cs

flatUpdate :: TableName -> [PrimExpr] -> Assoc -> FlatDB -> FlatDB
flatUpdate n cs u = flatTrace ("Update: " ++ n ++ ", " ++ show cs ++ ", " ++ show u) $ 
                    modifyTable n (relMap f)
    where p r = all (\c -> toBool (evalExpr r c)) cs
          f r | p r = [maybe e ((,) n . evalExpr r) $ lookup n u | e@(n,_) <- r]
              | otherwise = r

flatTables :: FlatDB -> [TableName]
flatTables = Map.keys

flatDescribe :: TableName -> FlatDB -> [(Attribute,FieldDesc)]
flatDescribe n = relSchema . fromJust . Map.lookup n

flatCreateTable :: TableName -> [(Attribute,FieldDesc)] -> FlatDB -> FlatDB
flatCreateTable n s = Map.insert n t -- FIXME: overwrites existing table
    where t = FlatRel { relSchema = s, relRows = [] }

flatDropTable :: TableName -> FlatDB -> FlatDB
flatDropTable n = Map.delete n

evalBinds :: FlatRow -> [(Attribute,PrimExpr)] -> FlatRow
evalBinds env bs = [(n,evalExpr env e) | (n,e) <- bs]

evalExpr :: FlatRow -> PrimExpr -> Value
evalExpr env (AttrExpr a) = fromJust $ lookup a env
evalExpr env (BinExpr op x1 x2) = 
    case op of
      OpEq      -> VBool (v1 == v2)
      OpLt      -> VBool (v1 <  v2)
      OpLtEq    -> VBool (v1 <= v2)
      OpGt      -> VBool (v1 > v2)
      OpGtEq    -> VBool (v1 >= v2)
      OpNotEq   -> VBool (v1 /= v2)
      OpAnd     -> VBool (toBool v1 && toBool v2)
      OpOr      -> VBool (toBool v1 || toBool v2)
      OpLike    -> VBool (toString v1 `matches` toString v2)
      OpIn      -> let v2s = case x2 of 
                               ListExpr x2s -> map (evalExpr env) x2s
                    in VBool (v1 `elem` v2s)
      OpOther o -> notImplemented $ show op
      OpCat     -> VString (toString v1 ++ toString v2)
      OpPlus    -> numOp (+) v1 v2
      OpMinus   -> numOp (-) v1 v2
      OpMul     -> numOp (*) v1 v2
      OpDiv     -> case (v1,v2) of
                     (VInteger i1, VInteger i2) -> VInteger (i1 `div` i2)
                     (VDouble  d1, VDouble  d2) -> VDouble (d1 / d2)
      OpMod     -> numOp (mod) v1 v2
      OpBitNot  -> notImplemented $ show op -- FIXME: shouldn't this be an UnOp?
      OpBitAnd  -> bitOp (.&.) v1 v2
      OpBitOr   -> bitOp (.|.) v1 v2
      OpBitXor  -> bitOp (xor) v1 v2
      OpAsg     -> notImplemented $ show op -- FIXME: wtf is this?
    where v1 = evalExpr env x1
          v2 = evalExpr env x2
          numOp o (VInteger i1) (VInteger i2) = VInteger (o i1 i2)
          numOp _ _ _ = error "numOp"
          bitOp o (VInteger i1) (VInteger i2) = VInteger (o i1 i2)
          bitOp _ _ _  = error "bitOp"
          matches = notImplemented "matches"

evalExpr env (UnExpr op x) =
    case op of
      OpNot       -> VBool (not (toBool v))
      OpIsNull    -> VBool (isNull v)
      OpIsNotNull -> VBool (not (isNull v))
      OpLength    -> VInteger (genericLength (toString v))
      UnOpOther o -> notImplemented $ show op
  where v = evalExpr env x
evalExpr env (AggrExpr op x) = undefined -- this need something different,
                                        -- it affects the result set
evalExpr env (ConstExpr c) = 
    case c of
      NullLit -> VNull
      DefaultLit -> notImplemented $ show c -- FIXME: need to know default for column
      BoolLit b -> VBool b
      StringLit s -> VString s
      IntegerLit i -> VInteger i
      DoubleLit d -> VDouble d
      DateLit d -> VDate d
      OtherLit l -> notImplemented $ show c
evalExpr env (CaseExpr cs el) =
    case [x | (c,x) <- cs, toBool (evalExpr env c)] of
      x:_ -> evalExpr env x
      _   -> evalExpr env el

evalAggr :: [FlatRow] -> PrimExpr -> Value
evalAggr rs (AggrExpr op e) = 
    case op of
      AggrCount   -> VInteger (genericLength vs) -- FIXME: should this count unique values?
      AggrSum     -> numAggr sum sum
      AggrAvg     -> dblAggr average
      AggrMin     -> numAggr minimum minimum
      AggrMax     -> numAggr maximum minimum
      AggrStdDev  -> dblAggr stddev
      AggrStdDevP -> dblAggr stddevp
      AggrVar     -> dblAggr variance
      AggrVarP    -> dblAggr variancep
      AggrOther o -> notImplemented $ show op
    where vs = map (\r -> evalExpr r e) rs
          numAggr :: ([Integer] -> Integer) -> ([Double] -> Double) -> Value
          numAggr f g | null vs || isInteger (head vs) = VInteger $ f $ map toInteger vs
                      | otherwise = VDouble $ g $ map toDouble vs
          dblAggr :: ([Double] -> Double) -> Value
          dblAggr f = numAggr (round . f . map fromIntegral) f
          average xs = sum xs / len xs
          stddev = sqrt . variance
          stddevp = sqrt . variancep
          variance xs =  sum [(x - average xs) ^ 2 | x <- xs] / (len xs - 1)
          variancep xs = sum [(x - average xs) ^ 2 | x <- xs] / len xs
          len = fromIntegral . length


-- Type checking

inferType :: [(Attribute,FieldDesc)] -> PrimExpr -> FieldDesc
inferType env e = undefined

-- Getting values from results

flatGetField :: (Value -> a) -> FlatRow -> String -> Maybe a
flatGetField f r n = 
    case lookup n r of
      Nothing    -> error $ "No field " ++ n ++ " in " ++ show r
      Just VNull -> Nothing
      Just v     -> Just $ f v

isNull :: Value -> Bool
isNull VNull = True
isNull _     = False

isInteger :: Value -> Bool
isInteger (VInteger _) = True
isInteger _ = False

toString :: Value -> String
toString (VString s) = s
toString v = error $ show v ++ " is not a string"

toInt :: Value -> Int
toInt (VInteger i) = fromIntegral i
toInt v = error $ show v ++ " is not an integer"

toInteger :: Value -> Integer
toInteger (VInteger i) = i
toInteger v = error $ show v ++ " is not an integer"

toDouble :: Value -> Double
toDouble (VDouble d) = d
toDouble v = error $ show v ++ " is not a double"

toBool :: Value -> Bool
toBool (VBool b) = b
toBool v = error $ show v ++ " is not a boolean"

toDate :: Value -> CalendarTime
toDate (VDate d) = d
toDate v = error $ show v ++ " is not a date"

notImplemented s = error $ "NOT IMPLEMENTED: " ++ s