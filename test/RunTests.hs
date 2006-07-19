module Main where

import DBTest
import TestCases

import Test.HUnit

import Control.Exception 
import Control.Monad
import Prelude hiding (catch)
import System.Environment
import System.Exit
import System.IO

configFile :: FilePath
configFile = "test.config"

getDatabases :: [String] -> IO ([Conn],[Conn])
getDatabases dbns = 
    do x <- liftM reads $ readFile configFile
       case x of
         [(dbs,"")] -> partitionM dbOK $ filter keepDB dbs
         _          -> fail $ "Parse error in " ++ configFile
  where keepDB db | null dbns = True
                  | dbLabel db `elem` dbns = True
                  | otherwise = False

dbOK :: Conn -> IO Bool
dbOK db = catch (withDB (\_ -> return True) db) f
    where 
      f :: Exception -> IO Bool
      f e = do hPutStrLn stderr $ "Problem with " ++ dbLabel db ++ ":"
               hPutStrLn stderr $ show e
               hPutStrLn stderr $ "Skipping " ++ dbLabel db
               return False

runDBTest :: [String] -> ([Conn] -> Test) -> IO Counts
runDBTest dbns f = 
    do (dbs,fdbs) <- getDatabases dbns
       cs <- runTestTT (f dbs)
       when (not (null fdbs)) $
            hPutStrLn stderr $ "Skipped databases: " ++ show (map dbLabel fdbs)
       return cs

main :: IO ()
main = do args <- getArgs
          c <- runDBTest args tests
          if errors c > 0 || failures c > 0  
             then exitFailure
             else exitWith ExitSuccess

-- Utilities

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM _ [] = return ([],[])
partitionM f (x:xs) = do b <- f x
                         (ts,fs) <- partitionM f xs
                         return $ if b then (x:ts,fs) else (ts,x:fs)