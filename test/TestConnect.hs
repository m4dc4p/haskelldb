module TestConnect where

import Database.HaskellDB
import Database.HaskellDB.GenericConnect
import System.Environment

connect :: [String] -> (Database -> IO a) -> IO a
connect (driver:args) = genericConnect driver args
connect _ = error "No driver argument supplied"

argConnect :: (Database -> IO a) -> IO a
argConnect f = do
               args <- getArgs
               connect args f