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

module LexToGeni (headerGen, formatLexGen)

where

import Dtypes
import System.Time
import Data.List


-- normaliazed functions for pretty printing
headerGen :: ClockTime -> String -> String
headerGen time pname =
    headerGeni time pname

formatLexGen :: [LexEntry] -> ClockTime -> String -> String -> String -> String -> String
formatLexGen lex _ _ _ _ _ =
    formatLexGeni lex
----------


headerGeni :: ClockTime -> String -> String
headerGeni _ pname = "%% lexicon generated by "++pname


formatLexGeni :: [LexEntry] -> String
formatLexGeni = concatMap convertEntry . sort

convertEntry :: LexEntry -> String
convertEntry e =
    lemma e ++" "++ family e ++"  "++ getParams e ++"\n"
    ++"equations:["
    ++ concatMap convertEqua (equations e)
    ++ concatMap convertCoanchor (coanchors e)
    ++ convertFS (iface e) ++"]\n"
    ++"filters:[" ++ convertFil (family e) (filters e) ++"]\n"
    ++"semantics:[" ++ convertSem (sem e) ++"]\n\n"


getParams :: LexEntry -> String
getParams e =
    case params e of
     [] -> ""
     _  -> "%("++(unwords $ map convertVal (params e))++")"


convertEqua :: Equa -> String
convertEqua (node,feat,val)  =
    let (fs, sn) = feat
        in
        case fs of "" -> case node of "anchor" -> node++"."++"bot"++"."++sn++":"++convertVal val++" "
                                      "anc"    -> "anchor"++"."++"bot"++"."++sn++":"++convertVal val++" "
                                      _ -> node++"."++sn++":"++convertVal val++" "
                   _  -> node++"."++fs++"."++sn++":"++convertVal val++" "


convertVal :: Val -> String
convertVal v =
    case v of
      Const (x:y:xs) -> concat $ intersperse "|" $ (x:y:xs)
      Var y          -> "?"++y
      Const [x]      -> x
      Anonymous      -> "_"
      Const []       -> error "empty constant"


convertCoanchor :: Coanchor -> String
convertCoanchor (node,lex,cat) =
    node++".top.lex:"++lex++" "++
    node++".top.cat:"++cat++" "


convertFS :: FS -> String
convertFS = unwords . map convertAttVal

convertAttVal :: AVPair -> String
convertAttVal (a,v) =
    "interface."++a++":"++convertVal v


convertFil :: String -> FS -> String
convertFil fam fil =
    "family:"++fam++" "++convertFS fil

convertSem :: Sem -> String
convertSem = concatMap convertLit


convertLit :: Lit -> String
convertLit (l,p,as) =
    convertVal l++":"++convertVal p++"("++(unwords $ map convertVal as)++") "
