import Database.HaskellDB
import Database.HaskellDB.HDBRec
import Database.HaskellDB.Database (GetRec)

import System.Time

import CustomSql
import TestConnect

timeQ = project (timefield << now)

ilikeQ s1 s2 = project (boolfield << s1 `ilike` s2)

getOneField f db q = do
		     [r] <- query db q
		     return (r!f)

getTime = getOneField timefield

getBool = getOneField boolfield


printResults :: ShowRecRow r => [r] -> IO ()
printResults = mapM_ (putStrLn . unwords . map (($ "") . snd) . showRecRow)

printSql = putStrLn . show . showSql

test db = do
	  printSql timeQ
	  getTime db timeQ >>= putStrLn . calendarTimeToString
	  let iq = ilikeQ (constant "apa") (constant "APA")
	  printSql iq
	  getBool db iq >>= putStrLn . show

main = argConnect test