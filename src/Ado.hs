-----------------------------------------------------------
-- Daan Leijen (c) 1999, daan@cs.uu.nl
--
-- This is a driver for Active Data Objects, a COM wrapper
-- around ODBC, available on any Win32 platform.
-- Recommended way of connecting is via DSN in the
-- start/settings/odbc dialog box.
--
-- Note that HaskellScript or H/Direct should be installed
-- in order to have the appropiate COM libraries.
-----------------------------------------------------------
module Ado ( adoRun
	   , adoConnect
	   
	   , AdoOptions(..)
	   , adoOptions, adoDSN, adoJet, adoSQL, adoOracle, localServer
	   
	   , Ado, AdoRow
	   
	   , adoPrimConnect, adoPrimCommand, adoPrimQuery
	   
	   , Date		--from Automation
	   ) where

import Pointer		(Pointer)
import Automation 
import AdoDB    hiding  (delete,update)
import IOExts           (unsafePerformIO)
import LibRef

import PrimQuery
import Query  (Rel,Attr,attributeName,scheme) 
import Database
import Sql


adoRun  = coRun

-----------------------------------------------------------
-- Options for connecting to an ADO database
-----------------------------------------------------------

data AdoOptions = AdoOptions { provider :: String       
                             , driver   :: String
                             , server   :: String
                             , dsn      :: String       --name binding in ODBC
                             , filedsn  :: String       
                             , dbase 	:: String       --explicit database
                             , uid      :: String       --user id
                             , pwd      :: String       --password
                             , other    :: String       --any device specific options
                  	     }           
                             
adoOptions      = AdoOptions { provider = "",   --default = ODBC 
                               driver = "", 
                               server = "", 
                               dsn="", 
                               filedsn="", 
                               dbase="",
                               uid="",
                               pwd="",
                               other=""}


-- adoDSN is the preferred way of connecting to ODBC databases, for example MS Access
-- or SQL server. Add your database to the ODBC DNS (click 'ODBC' in the 'settings' folder)
-- to use this kind of connection.
-- if you don't use DSN, you need to specify a "driver", "server" and "dbase"
-- the user id ("uid") and password ("pwd") can be given in both cases

adoDSN name     = adoOptions { dsn = name }

localServer	= "(local)"
adoSQL 		= adoOptions { driver = "{SQL Server}" }
adoOracle 	= adoOptions { provider = "MSDAORA" }
adoJet          = adoOptions { provider = "Microsoft.Jet.OLEDB.3.51" }

                               
instance Show AdoOptions where
  showsPrec d (AdoOptions { provider, driver, server, 
                            dsn, filedsn, dbase, 
                            uid, pwd, other })
        = showString    $
          concat        $
          map showOption [("provider",provider),("driver",driver),("server",server),
                          ("dsn",dsn),("filedsn",filedsn),("database",dbase),
                          ("uid",uid),("pwd",pwd),("",other)]
        where
          showOption (attr,val)  | null val     = ""
                                 | null attr    = val ++ ";"                     
                                 | otherwise    = attr ++ "=" ++ val ++ ";"

-----------------------------------------------------------
-- Basic data types
--
-- Ado holds a connection to an Ado database
-- AdoRow holds a returned record from an ado database.
-----------------------------------------------------------
type Ado	= Database (IConnection ()) (AdoRow)

data AdoRow r   = AdoRow [(Attribute,AdoValue)]
                | AdoRowKind (Rec r)

-- Enable selection in an Ado row
instance Variant a => Row AdoRow a where
  rowSelect = adoRowSelect


-----------------------------------------------------------
-- Make a connection,
-- 'newAdo' creates the actual Ado driver
-----------------------------------------------------------     

adoConnect :: AdoOptions -> (Ado -> IO a) -> IO a
adoConnect options action
    = adoPrimConnect (show options) action

adoPrimConnect :: String -> (Ado -> IO a) -> IO a
adoPrimConnect optionStr action
    = do{ connection <- coCreateObject "ADODB.Connection" iidIConnection
        ; connection # open optionStr ""  "" (-1)
        ; x <- action (newAdo connection)
        ; connection # close
        ; return x
	}


          
newAdo :: IConnection () -> Ado
newAdo connection
    = Database { dbQuery	= adoQuery
    	       , dbInsert	= adoInsert
	       , dbInsertNew 	= adoInsertNew
	       , dbDelete	= adoDelete
	       , dbUpdate	= adoUpdate
	       , database	= connection
	       }


-----------------------------------------------------------
-- Read values from an Ado row.
--
-- An 'AdoValue' is either a pointer to a VARIANT which
-- we should read or we have already read it.
-- mmmm, can this be done simpler and safer ??
-- I think that the 'ref' stuff can be replaced by
-- a suspended 'unsafePerformIO' action.
-----------------------------------------------------------
data AdoValue   = forall a.AdoValue (Ref (Either (Pointer VARIANT) a))

adoRowSelect :: Variant a => Attr r a -> AdoRow r -> a
adoRowSelect attr (AdoRow vals)
        = case (lookup (attributeName attr) vals) of
            Nothing   -> error "Query.rowSelect: invalid attribute used ??"
            Just val  -> unsafePerformIO $
                         do{ let r = getVal val
                           ; px <- getRef r
                           ; case px of
                              (Left p) -> do{ x <- readVariants1 resVariant p
                                            ; setRef r (Right x)
                                            ; return x
                                            }
                              (Right x)-> return x
                           }

getVal :: AdoValue -> Ref (Either (Pointer VARIANT) a)
getVal (AdoValue r)     = unsafeCoerce r

primitive unsafeCoerce "primUnsafeCoerce" :: a -> b



-----------------------------------------------------------
-- Insert(New)
-----------------------------------------------------------

adoInsertNew connection table assoc
	= adoPrimCommand connection sql
	where
	  sql	= show (ppInsert (toInsertNew table assoc))
	  
adoInsert connection table assoc
	= adoPrimCommand connection sql
	where
	  sql	= show (ppInsert (toInsert table assoc))
	  
-----------------------------------------------------------
-- Delete
-----------------------------------------------------------	  

adoDelete connection table exprs
	= adoPrimCommand connection sql    		
	where
	  sql	= show (ppDelete (toDelete table exprs))
	  
-----------------------------------------------------------
-- Update
-----------------------------------------------------------	  
	  
adoUpdate connection table criteria assigns
	= adoPrimCommand connection sql
	where
	  sql	= show (ppUpdate (toUpdate table criteria assigns))	  
	  
-----------------------------------------------------------
-- Query
-----------------------------------------------------------

adoQuery :: IConnection () -> PrimQuery -> Rel r -> IO [AdoRow r]
adoQuery connection qtree rel
    = adoPrimQuery connection sql scheme rel
    where
      sql 	= show (ppSql (toSql qtree))  
      scheme	= attributes qtree
      
      
-----------------------------------------------------------
-- Primitive Command
-----------------------------------------------------------      
adoPrimCommand :: IConnection () -> String -> IO ()
adoPrimCommand connection sql
	| null sql  = return ()
	| otherwise = do{ connection # execute0 sql (fromEnum AdCmdText)
	    		; return ()
	    		}

-----------------------------------------------------------
-- Primitive Query
-- The "Rel r" argument is a phantom argument to get
-- the return type right.
-----------------------------------------------------------

adoPrimQuery :: IConnection () -> String -> Scheme -> Rel r -> IO [AdoRow r]
adoPrimQuery connection sql scheme r
    = do{ (x,recordset) <- connection # execute0 sql (fromEnum AdCmdText)
        ; recordset # lazyRows action
        }
    where
      action fields = do{ xs <- sequence (map (readField fields) scheme)                        
                        ; return (AdoRow xs)
                        }



-----------------------------------------------------------
-- Read Fields
-----------------------------------------------------------

readField :: IFields a -> Attribute -> IO (Attribute,AdoValue)
readField fields name
     = do{ field  <- fields # getField name
         ; dispid <- field # getMemberID "Value"
         ; p      <- field # invokePropertyGet dispid [] [inEmpty defaultEmpty]
         ; r      <- newRef (Left p)
         ; return (name,AdoValue r)
        }
        
getFieldValue name = getField name ## getValue0         
getField name      = getItem1 name


-----------------------------------------------------------
-- Retrieve rows lazily
-----------------------------------------------------------

lazyRows action rs 
    = do { islast <- rs # getEOF
         ; if (boolFromInt islast)
            then last
            else first
         }
    where
       first    = do { rs # moveFirst; rows }
                   
       next     = nilOnError $
                  do { rs # moveNext;  rows }
       
       last     = nilOnError $
                  do { rs # close0; return [] }
       
       rows     = do { islast <- rs # getEOF
                     ; if (boolFromInt islast)
                        then last
                        else do { row <- rs # getFields
                                ; x   <- action row
                                ; return (x : unsafePerformIO next)
                                }
                     }
                     
       nilOnError io
                = io `catch` \err ->
                  do { putStr ("\n" ++ coGetErrorString err)
                     ; return []
                     }
                     
           