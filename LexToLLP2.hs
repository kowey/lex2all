module LexToLLP2 (headerLLP, formatLexLLP)

where

import Dtypes
import Generic
import Data.List
import System.Time
import XMLprint


-- normaliazed functions for pretty printing
headerLLP :: ClockTime -> String -> String
headerLLP _ _ =
    headerLLP2

formatLexLLP :: [LexEntry] -> ClockTime -> String -> String -> String -> String -> String
formatLexLLP lex time file _ _ _ =
    printXML (formatLexLLP2 lex time file) ""
-------


headerLLP2 :: String
headerLLP2 = 
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"++"<!DOCTYPE tagml SYSTEM \"tag.dtd\">\n\n"


formatLexLLP2 :: [LexEntry] -> ClockTime -> String -> XMLelem
formatLexLLP2 lex t file =
    let entries = sort lex
	time = calendarTimeToString (toUTCTime t)
	in 
	Elem {tag      = "tagml",
	      features = [],
	      datas    = "",
	      children = [Elem {tag      = "adminGrp",
				features = [],
				datas    = "",
				children = [
					    Elem {tag      = "admin",
						  features = [("type","content")],
						  datas    = "A lexicalizationLib generated from "++file,
						  children = []},
					    Elem {tag      = "admin",
						  features = [("type","organization")],
						  datas    = "LORIA - Nancy - France",
						  children = []},
					    Elem {tag      = "admin",
						  features = [("type","date")],
						  datas    = time,
						  children = []}]},
			  Elem {tag      = "lexicalizationLib",
				features = [("id",file)],
				datas    = "",
				children = 
				map (\x -> formatLexEnt x) entries }]}


-- converting function for a lexical entry
formatLexEnt :: LexEntry -> XMLelem
formatLexEnt e =
    Elem { tag      = "lexicalization",
	   features = [],
	   datas    = "",
	   children = 
	   [Elem {tag      = "tree", 
		  features = [],
		  datas    = "",
		  children = [Elem {tag      = "fs",
				    features = [],
				    datas    = "",
				    children = [Elem {tag      = "f",
						      features = [("name","family")],
						      datas    = "",
						      children = [Elem {tag      = "sym",
									features = [("value",(family e))],
									datas    = "",
									children = []}]}]}]},
	    Elem {tag      = "anchor",
		  features = [("noderef","anchor")],
		  datas    = "",
		  children = [Elem {tag      = "lemmaref",
				    features = [("name",(lemma e)), ("cat",(cat e))],
				    datas    = "",
				    children = []}]}]++
	   ((map (\x -> convertCoanchor x) (coanchors e))++
	    (map (\x -> convertEqua x) (equations e)))
	 }


convertCoanchor :: Coanchor -> XMLelem
convertCoanchor c =
    Elem {tag      = "anchor",
	  features = [("noderef",fst3 c)],
	  datas    = "",
	  children = [Elem {tag      = "lemmaref",
			    features = [("name",snd3 c),("cat",thd3 c)],
			    datas    = "",
			    children =[]}] }


convertEqua :: Equa -> XMLelem
convertEqua e =
    let (fs,sn) = (snd3 e)
        -- to fix: if fs is empty, _and_ if it is an anchor we should say bot (?)
        avm = case fs of "" -> "bot"
                         _  -> fs
        in
        Elem {tag      = "equation",
	                 features = [("type",avm),("noderef", fst3 e)],
	                 datas    = "",
	                 children = [Elem {tag      = "fs",
			                   features = [],
			                   datas    = "",
			                   children = [Elem {tag      = "f",
					                     features = [("name",sn)],
					                     datas    = "",
					                     children = [convertVal (thd3 e)]}]}]}


convertVal :: Val -> XMLelem
convertVal v =
    case v of Const (x:y:xs) -> Elem {tag      = "vAlt",
				      features = [],
				      datas    = "",
				      children = 
				      map (\e -> convertVal (Const [e])) (x:y:xs)} 
	      _ ->
-- 		  Elem {tag      = "sym",
-- 			features = [],
-- 			datas    = (show v),
-- 			children = []}
		  Elem {tag      = "sym",
			features = [("value",(show v))],
			datas    = "",
			children = []}
