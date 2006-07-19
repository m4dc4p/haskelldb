import Database.HaskellDB.FlatDB

import System.Environment
import System.IO

main :: IO ()
main = do args <- getArgs
          case args of
            [f] -> newDB f
            _   -> hPutStrLn stderr "Usage: flatdb-create <file>"