module MorphToLLP2 (morphHeaderLLP, formatMorphLLP)

where

import Dtypes
import Data.List
import System.Time
import XMLprint

-- normalized interface
morphHeaderLLP :: ClockTime -> String -> String
morphHeaderLLP _ _ =
    morphHeaderLLP2

formatMorphLLP :: [MorphEntry] -> ClockTime -> String -> String -> String -> String -> String
formatMorphLLP mph _ _ _ _ _ =
    printXML (formatMorphLLP2 mph) ""
--------


morphHeaderLLP2 :: String
morphHeaderLLP2 = 
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"++"<!DOCTYPE tagml SYSTEM \"tag.dtd\">\n\n"


formatMorphLLP2 :: [MorphEntry] -> XMLelem
formatMorphLLP2 mph =
    let lex = (sort mph)
      in Elem{tag      = "tagml",
              features = [],
              datas    = "",
              children = [Elem {tag      = "morphLib",
		                features = [],
		                datas    = "",
		                children = 
		                map (\x -> convertEntry x) lex }]}


convertEntry :: MorphEntry -> XMLelem
convertEntry e =
    Elem {tag      = "morph",
	  features = [("lex", morph e)],
	  datas    = "",
	  children = [Elem {tag      = "fs",
			    features = [],
			    datas    = "",
			    children = map (\x -> convertFeat x) (morphFeats e)},
		      Elem {tag      = "lemmaref",
			    features = [("cat", mcat e),("name",lem e)],
			    datas    = "",
			    children = []
			   }]
	 }


convertFeat :: AVPair -> XMLelem
convertFeat x =
    Elem {tag      = "f",
	  features = [("name",fst x)],
	  datas    = "",
	  children = [convertVal (snd x)]}
	  

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
			features = [("value",show v)],
			datas    = "",
			children = []}
