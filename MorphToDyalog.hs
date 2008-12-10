module MorphToDyalog (morphHeaderDya, formatMorphDya)

where

import Dtypes
import Data.List
import System.Time
import XMLprint

-- normalized interface
morphHeaderDya :: ClockTime -> String -> String
morphHeaderDya _ _ =
    morphHeaderDyalog

formatMorphDya :: [MorphEntry] -> ClockTime -> String -> String -> String -> String -> String
formatMorphDya mph _ _ fam lem morph =
    printXML (formatMorph mph fam lem morph) ""
-------


morphHeaderDyalog :: String
morphHeaderDyalog = 
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\" ?>\n"++"<!DOCTYPE tag SYSTEM \"http://atoll.inria.fr/~clerger/tag.dtd,xml\">\n\n"


formatMorph :: [MorphEntry] -> String -> String -> String -> XMLelem
formatMorph mph fam lem morph =
    let lex = (sort mph)
	in Elem {tag      = "tag",
		 features = [("familyfile",fam),("lemmafile",lem),("lexfile",morph)],
		 datas    = "",
		 children = 
		 map (\x -> convertEntry x) lex }


convertEntry :: MorphEntry -> XMLelem
convertEntry e =
    Elem {tag      = "morph",
	  features = [("lex", morph e)],
	  datas    = "",
	  children = [Elem {tag      = "lemmaref",
			   features = [("cat", mcat e),("name",lem e)],
			   datas    = "",
			   children = [Elem {tag      = "fs",
					    features = [],
					    datas    = "",
					    children = map (\x -> convertFeat x) (morphFeats e)}]
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
