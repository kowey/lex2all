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
headerGeni t pname = 
    let time = calendarTimeToString (toUTCTime t)
    in "%% lexicon generated by "++pname++" at "++time++"\n\n"


formatLexGeni :: [LexEntry] -> String
formatLexGeni lex =
        let l = sort lex
        in concatMap (\x -> convertEntry x) l

convertEntry :: LexEntry -> String
convertEntry e =
    (lemma e)++" "++(family e)++"  "++(getParams e)++"\n"
    ++"equations:["
    ++(concatMap (\x -> convertEqua x) (equations e))
    ++(concatMap (\x -> convertCoanchor x) (coanchors e))
    ++(convertFS (iface e))++"]\n"
    ++"filters:["++(convertFil (family e) (filters e))++"]\n"
    ++"semantics:["++(convertSem (sem e))++"]\n\n"


getParams :: LexEntry -> String
getParams e = 
    case (params e) of [] -> ""
                       _  -> "%("++(unwords $ map (\x -> convertVal x) (params e))++")"


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
convertFS iface =
    unwords (map (\x -> convertAttVal x) iface)


convertAttVal :: AVPair -> String
convertAttVal (a,v) =
    "interface."++a++":"++convertVal v


convertFil :: String -> FS -> String
convertFil fam fil =
    "family:"++fam++" "++convertFS fil

convertSem :: Sem -> String
convertSem s =
    concatMap (\x -> convertLit x) s


convertLit :: Lit -> String
convertLit (l,p,as) =
    convertVal l++":"++convertVal p++"("++(unwords $ map (\x -> convertVal x) as)++") "
