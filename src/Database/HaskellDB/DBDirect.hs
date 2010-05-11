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
import Database.HaskellDB.DBSpec (dbToDBSpec, dbname)
import Database.HaskellDB.DBSpec.DBSpecToDBDirect (dbInfoToModuleFiles, )

import qualified Database.HaskellDB.DBSpec.PPHelpers as PP

import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo, )
import System.Environment (getArgs, getProgName, )
import System.Exit (exitFailure, )
import System.IO (hPutStrLn, stderr, )

import Control.Monad.Error () -- Monad instance for Either
import Control.Monad (when, )
import Data.List (intersperse, )


createModules :: String -> String -> Bool -> PP.MakeIdentifiers -> Database -> IO ()
createModules m dbName useBStrT mkIdent db =
    do
    putStrLn "Getting database info..."
    spec <- dbToDBSpec useBStrT mkIdent m db
    putStrLn "Writing modules..."
    dbInfoToModuleFiles "." m (spec {dbname = dbName})


data Flags =
   Flags {
      optHelp            :: Bool,
      optBoundedStrings  :: Bool,
      optIdentifierStyle :: PP.MakeIdentifiers
   }

options :: [OptDescr (Flags -> Either String Flags)]
options =
   Option ['h'] ["help"]
      (NoArg (\flags -> Right $ flags{optHelp = True}))
       "show options" :
   Option ['b'] ["bounded-strings"]
      (NoArg (\flags -> Right $ flags{optBoundedStrings = True}))
       "use bounded string types" :
   Option []    ["identifier-style"]
      (ReqArg (\str flags ->
          case str of
             "preserve"   -> Right $ flags{optIdentifierStyle = PP.mkIdentPreserving}
             "camel-case" -> Right $ flags{optIdentifierStyle = PP.mkIdentCamelCase}
             _ -> Left $ "unknown identifier style: " ++ str)
        "type")
       "<type> is one of [preserve, camel-case]" :
   []

parseOptions ::
   [Flags -> Either String Flags] -> Either String Flags
parseOptions =
   foldr (=<<)
      (Right $
       Flags {optHelp = False,
              optBoundedStrings = False,
              optIdentifierStyle = PP.mkIdentPreserving})

exitWithError :: String -> IO a
exitWithError msg =
   hPutStrLn stderr msg >>
   hPutStrLn stderr "Try --help option to get detailed info." >>
   exitFailure

dbdirect :: DriverInterface -> IO ()
dbdirect driver =
    do putStrLn "DB/Direct: Daan Leijen (c) 1999, HWT (c) 2003-2004,"
       putStrLn "           Bjorn Bringert (c) 2005-2007, Henning Thielemann (c) 2008"
       putStrLn ""

       argv <- getArgs
       let (opts, modAndDrvOpts, errors) = getOpt RequireOrder options argv
       when (not (null errors))
          (ioError . userError . concat $ errors)

       flags <-
          case parseOptions opts of
             Left errMsg -> exitWithError errMsg
             Right flags -> return flags

       when (optHelp flags)
          (showHelp driver >> exitFailure)

       case modAndDrvOpts of
          []  -> exitWithError "Missing module and driver options"
          [_] -> exitWithError "Missing driver options"
          [moduleName,dbname,drvOpts] ->
              do putStrLn "Connecting to database..."
                 connect driver
                    (splitOptions drvOpts)
                    (createModules moduleName dbname
                        (optBoundedStrings flags)
                        (optIdentifierStyle flags))
                 putStrLn "Done!"
          (_:_:restArgs) ->
              exitWithError ("Unnecessary arguments: " ++ show restArgs)



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
             "Usage: " ++ p ++ " [dbdirect-options] <module> <db-name> <driver-options>\n"
          footer = unlines $
             "" :
             "NOTE: You will probably have to specify the db name in both <driver-options> and <db-name>.  This is because the driver options are specific to each database." :
             "" :
             "module:          Module name without an extension" :
             ("driver-options:  " ++
                (concat . intersperse "," .
                 map (\(name,descr) -> name++"=<"++descr++">") .
                 requiredOptions) driver) :
             []
      hPutStrLn stderr $ (usageInfo header options ++ footer)
