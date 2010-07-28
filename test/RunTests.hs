{-# LANGUAGE CPP #-}
module RunTests (Conn(..), dbTestMain) where

import DBTest
import TestCases

import Test.HUnit

import Control.Exception 
import Control.Monad
import Prelude hiding (catch)
import System.Environment
import System.Exit
import System.IO

dbOK :: Conn -> IO Bool
dbOK db = catch (withDB (\_ -> return True) db) f
    where 
      f :: SomeException -> IO Bool
      f e = do hPutStrLn stderr $ "Problem with " ++ dbLabel db ++ ":"
               hPutStrLn stderr $ show e
               return False

dbTestMain :: Conn -> IO ()
dbTestMain db =
    do ok <- dbOK db
       if ok then do c <- runTestTT (tests db)
                     if errors c > 0 || failures c > 0  
                       then exitFailure
                       else exitWith ExitSuccess
             else exitFailure
