-----------------------------------------------------------
-- Daan Leijen (c) 1998, daan@cs.uu.nl
-- Pretty print module based on Phil Wadlers "prettier printer"
--
-- additions:
--
-- * class Pretty a where
--      pretty :: a -> Doc	
--      
-- * indent :: Doc -> Doc
-- indent is equal to "nest n" where n is the column of the
-- current output. 
--
-- > test        = letin [text "x = 2", text "y = x"] (text "y + 2")
-- > letin xs x  = text "let" <+> indent (braces (sepby semi xs)) $$ text "in" <+> x
--
-- gives for "putStr $ showPretty 80 test"
-- > let {x=2;y=x} 
-- > in y + 3
--
-- but for "putStr $ showPretty 10 test"
-- > let {x=2;
-- >      y=x}
-- > in y + 3
-----------------------------------------------------------

module PPrint 
        ( Doc
        , emptyDoc, nest, text, newline, line, indent, group

        , (<|>)
        , (<>), (<+>)
        , ($$), (</>)
        , showPretty
        , Pretty, pretty
        
        , sep, hsep, hcat, vcat
        , sepby, commas, list
        
        , brackets, lbracket, rbracket
        , parens, lparen, rparen
        , braces, lbrace, rbrace
        , quotes, quote, squotes, squote
        
        , comma, space, dot, backslash, semi, colon
        , char
        
        , htext, tab
        
        ) where

infixl 4 <|>
infixl 5 </>,$$
infixl 6 <>,<+>


tab             = nest 2
htext           = hsep . map text

list            = brackets . commas
commas          = sepby comma

sepby sep       = indent . group . f
                where
                  f []     = emptyDoc
                  f [d]    = d
                  f (d:ds) = d <> sep $$ f ds

sep             = foldr (</>) emptyDoc
hsep            = foldr (<+>) emptyDoc     
hcat            = foldr (<>)  emptyDoc

vcat []		= emptyDoc
vcat xs		= foldr1 ($$) xs

x <+> y         = x <> space <> y
x </> y         = x <> line <+> y
x $$ y          = x <> newline <> y

squotes         = enclose squote squote
quotes          = enclose quote quote
braces          = enclose lbrace rbrace
parens          = enclose lparen rparen
brackets        = enclose lbracket rbracket
enclose l r x   = l <> x <> r

lparen          = char '('
rparen          = char ')'
lbracket        = char '['
rbracket        = char ']'
lbrace          = char '{'
rbrace          = char '}'

squote          = char '\''
quote           = char '"'
semi            = char ';'
colon           = char ':'
comma           = char ','
space           = char ' '
dot             = char '.'
backslash       = char '\\'

char c          = text [c]

-----------------------------------------------------------
-- 
-----------------------------------------------------------

showPretty w x  = show (best w x)

instance Show Doc where
  showsPrec d x = shows (best 60 x)
  
instance Show SimpleDoc where
  showsPrec d x = layout x
  
class Pretty a where
  pretty        :: a -> Doc 
  prettyList    :: [a] -> Doc
  prettyList    = list . map pretty

instance Pretty a => Pretty [a] where
  pretty        = prettyList
  
instance Pretty () where
  pretty ()     = text "()"
  
instance Pretty Char where
  pretty c      = char c
  prettyList s  = text s
    
instance Pretty Int where
  pretty i      = text (show i)
  
instance Pretty Integer where
  pretty i      = text (show i)

instance Pretty Float where
  pretty i      = text (show i)

instance Pretty Double where
  pretty i      = text (show i)

instance (Pretty a,Pretty b) => Pretty (a,b) where
  pretty (x,y)  = parens (pretty x <> comma <> pretty y)

instance (Pretty a,Pretty b,Pretty c) => Pretty (a,b,c) where
  pretty (x,y,z)= parens (pretty x <> comma <> pretty y <> comma <> pretty z)

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing        = text "Nothing"
  pretty (Just x)       = text "Just" <+> pretty x
  
  
--instance Show a => Pretty a where
--  pretty = text . show

--instance Pretty a => Show a where
--  showsPrec d x = shows (pretty x)

-----------------------------------------------------------
-- 
-----------------------------------------------------------

data Doc        = Empty
                | Cat Doc Doc
                | Nest Int Doc
                | Text String
                | Line
                | Indent Doc
                | Union Doc Doc
                
data SimpleDoc  = SEmpty
                | SText String SimpleDoc
                | SLine Int SimpleDoc
                
                
emptyDoc        = Empty
x <> y          = Cat x y
nest i x        = Nest i x
text s          = Text s
line            = group newline
newline         = Line
indent x        = Indent x
x <|> y         = Union x y

group x         = flatten x <|> x

flatten Empty           = Empty
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Text s)        = Text s
flatten Line            = Empty
flatten (Indent x)      = (flatten x)
flatten (Union x y)     = flatten x

  
layout SEmpty           = id
layout (SText s x)      = showString s . shows x
layout (SLine i x)      = showString ('\n':replicate i ' ') . shows x

best w x        = best' 0 [(0,x)]
                where
                  best' k []            = SEmpty
                  best' k ((i,d):ds)    
                        = case d of
                            Empty       -> best' k ds                
                            Cat x y     -> best' k ((i,x):(i,y):ds)                
                            Nest j x    -> best' k ((i+j,x):ds)                
                            Text s      -> SText s (best' (k+length s) ds)                 
                            Line        -> SLine i (best' i ds)                 
                            Indent x    -> best' k ((i,Nest (k-i) x):ds)                  
                            Union x y   -> better w k (best' k ((i,x):ds))                
                                                      (best' k ((i,y):ds))                
                                                                      
better w k x y  | fits (w-k) x  = x
                | otherwise     = y
                
fits w x        | w < 0         = False
fits w SEmpty                   = True
fits w (SText s y)              = fits (w - length s) y
fits w (SLine i y)              = True


