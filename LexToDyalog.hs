module LexToDyalog (headerDya, formatLexDya)

where

import Dtypes
import Generic
import Data.List
import System.Time
import XMLprint


-- normaliazed functions for pretty printing
headerDya :: ClockTime -> String -> String
headerDya _ _ =
    headerDyalog

formatLexDya :: [LexEntry] -> ClockTime -> String -> String -> String -> String -> String
formatLexDya lex _ _ fam lem morph =
    printXML (formatLex lex fam lem morph) ""
-------


headerDyalog :: String
headerDyalog = 
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\" ?>\n"++"<!DOCTYPE tag SYSTEM \"http://atoll.inria.fr/~clerger/tag.dtd,xml\">\n\n"


formatLex :: [LexEntry] -> String -> String -> String -> XMLelem
formatLex lex fam lem morph =
    let keys = extractLemmaCats (sort lex)
	lexfam = buildNewLex keys lex
	in Elem {tag      = "tag",
		 features = [("axiom","s"),("familyfile",fam),("lemmafile",lem),("lexfile",morph)],
		 datas    = "",
		 children = 
		 map (\x -> formatLexFam x) lexfam }


-- the list of the lemmaCats without duplicates
-- this will be the key of our Dictionary
extractLemmaCats :: [LexEntry] -> [String]
extractLemmaCats l =
    nub (map (\x -> (lemma x)++(cat x)) l)


-- here we gather some entries according to their lemma and cat
buildNewLex :: [String] -> [LexEntry] -> [[LexEntry]]
buildNewLex s lex = 
    map (\y -> filter (\x -> ((lemma x)++(cat x)) == y) lex) s


-- converting function for a lexical family (same lemma and cat)
formatLexFam :: [LexEntry] -> XMLelem
formatLexFam f =
    let hd = head f
	in   Elem { tag      = "lemma",
		    features = [("name",lemma hd),("cat",cat hd)],
		    datas    = "",
		    children = 
		    map (\x -> convertEntry x) f }

convertEntry :: LexEntry -> XMLelem
convertEntry l =
    Elem {tag      = "anchor",
	  features = [("tree_id", "family[@name="++(family l)++"]")],
	  datas    = "",
	  children = 
	  (map (\x -> convertEqua (cat l) x) (equations l))++(map (\x -> convertCoanchor x) (coanchors l))}


convertEqua :: String -> Equa -> XMLelem
convertEqua cat e =
    let node = case (fst3 e) of "anchor" -> "anc"
                                _      -> (fst3 e)
        (fs,sn)= (snd3 e)
        -- to fix, when to say it is bot, if concerns an anchor ?
        avm = case fs of "" -> "bot"
                         _  -> fs
        nodeid = case node of "anc" -> cat++"_"++node
                              _ -> node
    in
    Elem {tag      = "equation",
	  features = [("type",avm),("node_id", nodeid)],
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
			features = [("value",show v)],
			datas    = "",
			children = []}

convertCoanchor :: Coanchor -> XMLelem
convertCoanchor c =
    Elem {tag      = "coanchor",
	  features = [("node_id",fst3 c)],
	  datas    = "",
	  children = [Elem {tag      = "lex",
			    features = [],
			    datas    = snd3 c,
			    children =[]}] }
