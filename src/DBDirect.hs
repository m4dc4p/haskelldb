-----------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Daan Leijen (c) 1999, daan@cs.uu.nl
--                HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- DBDirect generates a Haskell module from a database.
-- It first reads the system catalog of the database into
-- a 'Catalog' data type. After that it pretty prints that
-- data structure in an appropiate Haskell module which
-- can be used to perform queries on the database.
--
-- $Revision: 1.7 $
-----------------------------------------------------------

module Main where

import Data.Char
import System.Environment (getArgs)
import System.Directory
import Text.PrettyPrint.HughesPJ

import Database.HaskellDB
import Database.HaskellDB.GenericConnect
import Database.HaskellDB.DBSpec
import Database.HaskellDB.DBSpec.PPHelpers
import Database.HaskellDB.DBSpec.DBSpecToDBDirect

-- | Command line driver
main = do
       putStr "\nDB/Direct, Daan Leijen (c) 1999, HWT (c) 2003-2004\n\n"
       args <- getArgs
       putStrLn "checking arguments..."
       if (checkFlag $ args) then process True (tail args) 
	  else process False args
       where 
       process useBStrT args = 
	   case checkArgs args of
	    True -> do
		    let db = genericConnect (map toLower (head args)) 
			       (init $ tail args)
		    putStrLn "creating database specification..."
		    spec <- db (dbToDBSpec useBStrT (last args))
		    putStrLn "creating modules from specification..."
		    let spec' = finalizeSpec spec
			files = specToHDB spec'
		    putStrLn "writing modules..."
		    createModules files
		    putStrLn "done!"
	    False -> showHelp
       checkFlag []   = False
       checkFlag args = head args == "-b"
       checkArgs [] = False
       checkArgs args
	   = dbarg == "odbc" && (length args) == 5
	     || dbarg == "mysql" && (length args) == 6
             || (dbarg == "postgresql" || dbarg == "postgre") 
		    && (length args) == 6
             || dbarg == "sqlite" && (length args) == 4
             || (dbarg == "wxhaskell" || dbarg == "wx") 
		    && (length args) == 5
	   where dbarg = map toLower (head args)

-- | Shows usage information
showHelp 
    = putStr (unlines helpText)
      where
      helpText  = ["Wrong number of arguments!",
		   "We want:",
		   "Enable BoundedString [-b],",
		   "database type (ODBC, MySQL, PostgreSQL or SQLite) and",
		   "ODBC: dsn, userid, password and file",
		   "MySQL: server, database, userid password and file",
		   "PostgreSQL: server, database, userid, password and file",
		   "SQLite: filepath IOMode and file",
		   "WXHaskell: dsn, userid, password and file",
		   "as arguments"
	          ]
-- | Creates all modules
createModules :: [(FilePath,Doc)] -> IO ()
createModules files
    = do 
      let dbname = (fst . head) files
	  dbnamenohs = reverse $ drop 3 $ reverse dbname
      writeFile dbname (render $ (snd . head) files)
      createDirectory dbnamenohs
      mapM_ (\ (name,doc) -> writeFile name
	     (render doc)) (tail files)
