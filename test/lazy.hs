import Database.HaskellDB

import TestConnect

import Dp037.Lazy_test

{-
 Uses table created with:
 CREATE TABLE lazy_test (doc_id INT NOT NULL, body TEXT NOT NULL);
-}

docSize = 1000
numRecords = 10000
getRecords = 10
lazy = False

addDoc db bdy di = 
    insert db lazy_test (doc_id << constant di # body << constant bdy)

doc_body = replicate docSize 'x'

insertData db = mapM (addDoc db doc_body) [1..numRecords]

deleteData db = delete db lazy_test (\_ -> constant True)

countN db n = 
    do
    rs <- (if lazy then lazyQuery else strictQuery) db (table lazy_test)
    return (length (take n rs))

test db =
    do
    putStrLn $ "Deleting data..."
    deleteData db
    putStrLn $ "Adding " ++ show numRecords ++ " records..."
    insertData db
    putStrLn $ "Counting " ++ show getRecords ++ " "
		 ++ (if lazy then "lazy" else "strict") ++ " results ..."
    n <- countN db getRecords 
    putStrLn $ "n = " ++ show n
    putStrLn $ "Deleting data..."
    deleteData db

main = argConnect test
    