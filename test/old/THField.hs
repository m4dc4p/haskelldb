-----------------------------------------------------------
-- |
-- Module      :  Query
-- Copyright   :  HWT Group (c) 2003, dp03-7@mdstud.chalmers.se
-- License     :  BSD-style
-- 
-- Maintainer  :  dp03-7@mdstud.chalmers.se
-- Stability   :  experimental
-- Portability :  non-portable (requires Template Haskell)
--
-- Provides a Template Haskell function that declares
-- a HaskellDB field.
-- 
-- $Revision: 1.2 $
-----------------------------------------------------------
module THField (field, module Database.HaskellDB.DBLayout) where

import Database.HaskellDB.DBLayout
import Language.Haskell.THSyntax

-- | Declare a field.
field :: String  -- ^ Haskell identifier for the field (e.g. "xid")
      -> String  -- ^ Actual field name (e.g. "id")
      -> String  -- ^ Name of the field label type (e.g. "Id")
      -> Bool    -- ^ Whether the field is nullable
      -> String  -- ^ Name of the value type of the field (e.g. "Int")
      -> Q [Dec]
field attrName fieldName tagName nullable typeName 
    = return $ mkField attrName fieldName tagName nullable typeName 

mkField :: String -> String -> String -> Bool -> String -> [Dec]
mkField attrName fieldName tagName nullable typeName = 
    [
     mkTag tagName, 
     mkFieldTagInstance tagName fieldName, 
     mkAttrSig attrName tagName nullable typeName, 
     mkAttrVal attrName tagName
    ]

mkTag tagName = DataD [] tagName [] [NormalC tagName []] []

mkFieldTagInstance tagName fieldName = 
    InstanceD [] (AppT (ConT "FieldTag") (ConT tagName)) 
		  [FunD "fieldName" 
		   [Clause [WildP] (NormalB (LitE (StringL fieldName))) []]]

mkAttrSig attrName tagName nullable typeName 
    = SigD attrName (AppT (AppT 
			   (ConT "Attr") 
			   (ConT tagName)) (mkType nullable typeName))

mkType nullable typeName = if nullable then AppT (ConT "Maybe") t else t
    where t = ConT typeName

mkAttrVal attrName tagName = 
    ValD (VarP attrName) (NormalB 
			  (AppE (VarE "mkAttr") 
			   (ConE tagName))) []

