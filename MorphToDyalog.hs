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
