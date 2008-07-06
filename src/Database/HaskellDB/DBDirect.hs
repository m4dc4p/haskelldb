-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.DBDirect
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003,
--                Bjorn Bringert (c) 2005-2006, bjorn@bringert.net
-- License     :  BSD-style
-- 
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  portable
--
-- DBDirect generates a Haskell module from a database.
-- It first reads the system catalog of the database into
-- a 'Catalog' data type. After that it pretty prints that
-- data structure in an appropiate Haskell module which
-- can be used to perform queries on the database.
--
-----------------------------------------------------------

module Database.HaskellDB.DBDirect (dbdirect) where

import Database.HaskellDB (Database, )
import Database.HaskellDB.DriverAPI (DriverInterface, connect, requiredOptions, )
import Database.HaskellDB.DBSpec (dbToDBSpec, )
import Database.HaskellDB.DBSpec.DBSpecToDBDirect (dbInfoToModuleFiles, )

import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo, )
import System.Environment (getArgs, getProgName, )
import System.Exit (exitFailure, )
import System.IO (hPutStrLn, stderr, )

import Control.Monad (when, )
import Data.List (intersperse, )


createModules :: String -> Bool -> Database -> IO ()
createModules m useBStrT db = 
    do
    putStrLn "Getting database info..."
    spec <- dbToDBSpec useBStrT m db
    putStrLn "Writing modules..."
    dbInfoToModuleFiles "." m spec


data Flags =
   Flags {
      optHelp           :: Bool,
      optBoundedStrings :: Bool
     }

options :: [OptDescr (Flags -> Flags)]
options =
   Option ['h'] ["help"]
      (NoArg (\flags -> flags{optHelp = True}))
       "show options" :
   Option ['b'] ["bounded-strings"]
      (NoArg (\flags -> flags{optBoundedStrings = True}))
       "use bounded string types" :
   []

dbdirect :: DriverInterface -> IO ()
dbdirect driver = 
    do putStrLn "DB/Direct: Daan Leijen (c) 1999, HWT (c) 2003-2004,"
       putStrLn "           Bjorn Bringert (c) 2005-2007"
       putStrLn "           Henning Thielemann (c) 2008"
       putStrLn ""

       argv <- getArgs
       let (opts, modAndDrvOpts, errors) = getOpt RequireOrder options argv
       when (not (null errors))
          (ioError . userError . concat $ errors)
       let flags = foldr ($)
             (Flags {optHelp = False,
                     optBoundedStrings = False}) opts
       when (optHelp flags)
          (showHelp driver >> exitFailure)

       case modAndDrvOpts of
          [] -> putStrLn "Missing module and driver options, cf. --help"
          [_] -> putStrLn "Missing driver options, cf. --help"
          [moduleName,drvOpts] ->
              do putStrLn "Connecting to database..."
                 connect driver
                    (splitOptions drvOpts)
                    (createModules moduleName (optBoundedStrings flags))
                 putStrLn "Done!"
          (_:_:restArgs) ->
              putStrLn ("Unnecessary arguments: " ++ show restArgs ++ ", cf. --help")



splitOptions :: String -> [(String,String)]
splitOptions = map (split2 '=') . split ','

split :: Char -> String -> [String]
split _ [] = []
split g xs = y : split g ys
  where (y,ys) = split2 g xs

split2 :: Char -> String -> (String,String)
split2 g xs = (ys, drop 1 zs)
  where (ys,zs) = break (==g) xs

-- | Shows usage information
showHelp :: DriverInterface -> IO ()
showHelp driver =
   do p <- getProgName
      let header =
             "Usage: " ++ p ++ " [dbdirect-options] <module> <driver-options>\n\n" ++
             "driver-options: " ++
                (concat . intersperse "," .
                 map (\(name,descr) -> name++"=<"++descr++">") .
                 requiredOptions) driver ++ "\n"
      hPutStrLn stderr $ usageInfo header options
