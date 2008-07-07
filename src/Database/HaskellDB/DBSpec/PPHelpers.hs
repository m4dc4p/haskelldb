-----------------------------------------------------------
-- |
-- Module      :  PPHelpers
-- Copyright   :  HWT Group (c) 2004, haskelldb-users@lists.sourceforge.net
-- License     :  BSD-style
--
-- Maintainer  :  haskelldb-users@lists.sourceforge.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Various functions used when pretty printing stuff
--
--
-----------------------------------------------------------
module Database.HaskellDB.DBSpec.PPHelpers where
-- no explicit export, we want ALL of it

import Data.Char (toLower, toUpper, isAlpha, isAlphaNum, )
import Text.PrettyPrint.HughesPJ

newline = char '\n'

-----------------------------------------------------------
-- Comment that looks like this
-----------------------------------------------------------
ppComment txt
	= commentLine $$ vcat (map commentText txt) $$ commentLine
	where
	  commentLine	= text (replicate 75 '-')
	  commentText s	= text ("-- " ++ s)

-----------------------------------------------------------
-- Create valid Names
-----------------------------------------------------------
fileName name	| not (elem '.' baseName)  = name ++ ".hs"
		| otherwise		   = name
		where
	          baseName = reverse (takeWhile (/='\\') (reverse name))


data MakeIdentifiers =
   MakeIdentifiers
      { moduleName, identifier, toType :: String -> String }

mkIdentPreserving =
   MakeIdentifiers
      {
         moduleName = checkChars . checkUpper,
         identifier = checkChars . checkKeyword . checkLower,
         toType     = checkChars . checkKeyword . checkUpper
      }

mkIdentCamelCase =
   MakeIdentifiers
      {
         moduleName = checkChars . toUpperCamelCase,
         identifier = checkChars . checkKeyword . toLowerCamelCase,
         toType     = checkChars . checkKeyword . toUpperCamelCase
      }


toLowerCamelCase s@(_:_) =
   let (h : rest) = split ('_'==) $ dropWhile ('_'==) $ map toLower s
   in  concat $ checkLower h : map (checkUpperDef '_') rest
toLowerCamelCase [] =
   error "toLowerCamelCase: identifier must be non-empty"

toUpperCamelCase s@(_:_) =
   let (h : rest) = split ('_'==) $ dropWhile ('_'==) $ map toLower s
   in  concat $ checkUpper h : map (checkUpperDef '_') rest
toUpperCamelCase [] =
   error "toUpperCamelCase: identifier must be non-empty"

{- |
Generalization of 'words' and 'lines' to any separating character set.
-}
split :: Eq a => (a -> Bool) -> [a] -> [[a]]
split p =
   foldr (\ x yt@ ~(y:ys) -> (if p x then ([]:yt) else ((x:y):ys)) ) [[]]

checkChars s	= map replace s
		where
		  replace c	| isAlphaNum c	= c
		  		| otherwise	= '_'

checkKeyword s	| elem s keywords  = 'x' : s
		| otherwise	   = s
		where
		  keywords	= [ "module", "where", "import"
		  		  , "infix", "infixr", "infixl"
		  		  , "type", "newtype", "data"
		  		  , "deriving"
		  		  , "class", "instance"
		  		  , "do", "return"
		  		  , "let", "in"
		  		  , "case", "of"
		  		  , "if", "then", "else"
		  		  , "id", "zip","baseTable"
		  		  ]

checkUpper "" = error "Empty name from database?"
checkUpper s = checkUpperDef 'X' s

checkLower "" = error "Empty name from database?"
checkLower s = checkLowerDef 'x' s

checkUpperDef _ ""      = ""
checkUpperDef d s@(x:xs)
			| isAlpha x	= toUpper x : xs
			| otherwise	= d : s -- isDigit?

checkLowerDef _ ""      = ""
checkLowerDef d s@(x:xs)
			| isAlpha x	= toLower x : xs
			| otherwise	= d : s -- isDigit?
