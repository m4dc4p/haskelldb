-----------------------------------------------------------
-- |
-- Module      :  PPHelpers
-- Copyright   :  HWT Group (c) 2004, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Various functions used when pretty printing stuff
-----------------------------------------------------------
module Database.HaskellDB.DBSpec.PPHelpers where
-- no explicit export, we want ALL of it

import Data.Char
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
		  
		  
moduleName 	= checkChars . checkUpper
identifier	= checkChars . checkKeyword . checkLower
toType          = checkChars . checkKeyword . checkUpper

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

checkUpper ""           = error "Empty name from database?"
checkUpper s@(x:xs)	| isUpper x	= s
			| isLower x	= toUpper x : xs
			| otherwise	= 'X' : s -- isNumeric?

checkLower ""           = error "Empty name from database?"	
checkLower s@(x:xs)	| isLower x	= s
			| isUpper x	= toLower x : xs
			| otherwise	= 'x' : s -- isNumeric?	 
