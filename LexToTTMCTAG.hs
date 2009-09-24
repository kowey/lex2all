-- Copyright (C) 2009 Yannick Parmentier
--
-- This software is a computer program whose purpose is to [describe
-- functionalities and technical features of your software].
--
-- This software is governed by the CeCILL  license under French law and
-- abiding by the rules of distribution of free software.  You can  use,
-- modify and/ or redistribute the software under the terms of the CeCILL
-- license as circulated by CEA, CNRS and INRIA at the following URL
-- "http://www.cecill.info".
--
-- As a counterpart to the access to the source code and  rights to copy,
-- modify and redistribute granted by the license, users are provided only
-- with a limited warranty  and the software's author,  the holder of the
-- economic rights,  and the successive licensors  have only  limited
-- liability.
--
-- In this respect, the user's attention is drawn to the risks associated
-- with loading,  using,  modifying and/or developing or reproducing the
-- software by the user in light of its specific status of free software,
-- that may mean  that it is complicated to manipulate,  and  that  also
-- therefore means  that it is reserved for developers  and  experienced
-- professionals having in-depth computer knowledge. Users are therefore
-- encouraged to load and test the software's suitability as regards their
-- requirements in conditions enabling the security of their systems and/or
-- data to be ensured and,  more generally, to use and operate it in the
-- same conditions as regards security.
--
-- The fact that you are presently reading this means that you have had
-- knowledge of the CeCILL license and that you accept its terms.

module LexToTTMCTAG (headerTT, formatLexTT)

where

import Dtypes
import Generic
import Data.List
import System.Time
import XMLprint

-- normaliazed functions for pretty printing
headerTT :: ClockTime -> String -> String
headerTT _ _ =
    headerTTMCTAG

formatLexTT :: [LexEntry] -> ClockTime -> String -> String -> String -> String -> String
formatLexTT lex _ _ fam lem morph =
    printXML (formatLex lex fam lem morph) ""
-------


headerTTMCTAG :: String
headerTTMCTAG =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>\n"++"<!DOCTYPE mcgrammar SYSTEM \"xmg-mctag.dtd,xml\">\n\n"


formatLex :: [LexEntry] -> String -> String -> String -> XMLelem
formatLex lex _ _ _ =
    let keys = extractLemmaCats (sort lex)
	lexfam = buildNewLex keys lex
	in Elem {tag      = "mcgrammar",
		 features = [],
		 datas    = "",
		 children = [
                             Elem {tag      = "lemmas",
                                   features = [],
                                   datas    = "",
                                   children =
                                       map (\x -> formatLexFam x) lexfam }
                            ]}


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
                    (map (\x -> convertEntry x) f)}


convertEntry :: LexEntry -> XMLelem
convertEntry l =
    Elem {tag      = "anchor",
	  features = [("tree_id", "family[@name="++(family l)++"]")],
	  datas    = "",
	  children =
	  [(convertFilter (filters l))]++(map (\x -> convertEqua (cat l) x) (equations l))++(map (\x -> convertCoanchor x) (coanchors l))++[(convertSem (calls l))]}


convertFilter :: FS -> XMLelem
convertFilter fi =
    Elem {tag      = "filter",
	  features = [],
	  datas    = "",
	  children = [(convertFS fi)]
         }

convertSem :: SemCall -> XMLelem
convertSem sem =
    Elem {tag      = "sem",
          features = [],
          datas    = "",
          children =
          (map (\x -> convertMacro x) sem)}

convertMacro :: MacroCall -> XMLelem
convertMacro macro =
    Elem {tag      = "semclass",
          features = [("name",fst macro)],
          datas    = "",
          children = [(convertArgs (snd macro))]}

convertArgs :: FS -> XMLelem
convertArgs args =
    Elem {tag      = "args",
          features = [],
          datas    = "",
          children =
          (map (\x -> convertFeat x) args)}


convertFS :: FS -> XMLelem
convertFS fs =
    Elem {tag      = "fs",
	  features = [],
	  datas    = "",
	  children =
          (map (\x -> convertFeat x) fs)}


convertFeat :: AVPair -> XMLelem
convertFeat feat =
    let (fs, sn) = (fst feat, snd feat)
    in
    Elem {tag      = "f",
	  features = [("name", fs)],
	  datas    = "",
	  children = [convertVal sn]}


convertEqua :: String -> Equa -> XMLelem
convertEqua _ e =
    let nodeid = (fst3 e)
        (fs,sn)= (snd3 e)
        -- by default the equation unifies with bot FS
        avm = case fs of "" -> "bot"
                         _  -> fs
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
		  Elem {tag      = "sym",
			features = [("value",show v)],
			datas    = "",
			children = []}

convertCoanchor :: Coanchor -> XMLelem
convertCoanchor c =
    Elem {tag      = "coanchor",
	  features = [("node_id",fst3 c)]++[("cat", thd3 c)],
	  datas    = "",
	  children = [Elem {tag      = "lex",
			    features = [],
			    datas    = snd3 c,
			    children =[]}] }
