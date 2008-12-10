module MorphToTTMCTAG (formatMorphTT)

where

import Dtypes
import Data.List
import System.Time
import XMLprint

-- normalized interface
formatMorphTT :: [MorphEntry] -> ClockTime -> String -> String -> String -> String -> String
formatMorphTT mph _ _ fam lem morph =
    printXML (formatMorph mph fam lem morph) ""
-------

formatMorph :: [MorphEntry] -> String -> String -> String -> XMLelem
formatMorph mph _ _ _ =
    let lex = (sort mph)
	in Elem {tag      = "mcgrammar",
		 features = [],
		 datas    = "",
		 children = [
                             Elem {tag      = "morphs",
                                   features = [],
                                   datas    = "",
                                   children = map (\x -> convertEntry x) lex }
                            ]}
                            

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
