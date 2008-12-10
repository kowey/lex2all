module XMLprint (raw, printXML)

where

import Dtypes

raw :: String -> String 
raw x = x

indent :: String 
indent  = "    "

push :: String -> String
push s = s++indent

--pop :: String -> String
--pop s = take ((length s) - (length indent)) s

printXML :: XMLelem -> String -> String
printXML e i =
    case e of EmptyElem -> ""
	      Elem {tag      = t,
		    features = f,
		    datas    = d,
		    children = c} -> 
		       i++"<"++t++(concat (map (\(x,y) -> " "++x++"=\""++y++"\" ") f))++(if d /= "" then ">"++d++"</"++t++">\n" else (if not (null c) then let ind=(push i) in ">\n"++(concat (map (\x -> printXML x ind) c))++i++"</"++t++">\n" else "/>\n"))
