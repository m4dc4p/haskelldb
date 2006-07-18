module Main where

import DBTest
import TestCases

import Test.HUnit

import Control.Exception 
import Control.Monad
import Prelude hiding (catch)
import System.Exit
import System.IO

configFile :: FilePath
configFile = "test.config"

getDatabases :: IO [DB]
getDatabases = do x <- liftM reads $ readFile configFile
                  case x of
                    [(dbs,"")] -> filterM dbOK dbs
                    _          -> fail $ "Parse error in " ++ configFile

dbOK :: DB -> IO Bool
dbOK db = catch (withDB (\_ -> return True) db) f
    where 
      f :: Exception -> IO Bool
      f e = do hPutStrLn stderr $ "Problem with " ++ dbLabel db ++ ":"
               hPutStrLn stderr $ show e
               hPutStrLn stderr $ "Skipping " ++ dbLabel db
               return False

runDBTest :: ([DB] -> Test) -> IO Counts
runDBTest f = do dbs <- getDatabases
                 runTestTT (f dbs)

main :: IO ()
main = do c <- runDBTest tests
          if errors c > 0 || failures c > 0  
             then exitFailure
             else exitWith ExitSuccess