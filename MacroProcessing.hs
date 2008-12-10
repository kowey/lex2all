-- module containing functions operating on Sem Macros
-- (Dict building and instanciation)
module MacroProcessing (macrosToDict, evalueSemLex)

where

import Dtypes
import Data.List (nub)
import qualified Data.Map as Dict
import Generic

-- transformation of the list of macros into a dictionary (Mapping) 
-- where the macro names are the keys
macrosToDict :: [SemMacro] -> Macros
macrosToDict l = 
    let extract macros = (map (\x -> (name x,x)) macros)
	in Dict.fromList (extract l)


-- instanciation of the whole lexicon
evalueSemLex :: [LexEntry] -> Macros -> [LexEntry]
evalueSemLex l m =
    map (\x -> 
	 let allSem = (evalueSem x m) 
	     semX   = concat (map (\x -> fst3 x) allSem)
	     ifaceX = concat (map (\x -> snd3 x) allSem)
	     paramX = concat (map (\x -> thd3 x) allSem)
	 in Lex {
		 lemma     = lemma x,
		 cat       = cat x,
		 calls     = calls x,
		 params    = paramX,
		 sem       = semX,
		 iface     = ifaceX,
		 acc       = acc x,
		 family    = family x,
		 filters   = filters x,
		 except    = except x,
		 equations = equations x,
		 coanchors = coanchors x}) l


-- extract a macro from a name
findMacro :: Macros -> String -> SemMacro
findMacro m x =  
    case Dict.lookup x m of  
			 Nothing  -> error ("'" ++ x ++ "' not found in macros")
			 Just res -> res

-- conditional lookup
-- this performs alpha-conversion (via param i) to ensure consistency
-- while calling several semantic macros potentially sharing identifiers
find :: Int -> (Dict.Map String Val) -> Val -> Val
find i dict val = 
    case val of Anonymous -> val
		Const _   -> val
		Var x     -> 
		    let v = (Dict.findWithDefault Anonymous x dict) 
			in if v == Anonymous then  
			-- if the variable hasn't been specified inside
			-- the arguments of the macro, we let it unspecified
			Var (x++"_"++(show i))
			-- for debugging, uncomment below
			--error (x++" key not found")
			    else v 


-- evalue the semantic call from a lexicon entry
evalueSem :: LexEntry -> Macros -> [(Sem,FS,[Val])]
evalueSem e m = 
    let macros = (calls e)
	in if null macros then []
	   else mapInd (\x i -> evalueOneSem i x m) macros


evalueOneSem :: Int -> MacroCall -> Macros -> (Sem,FS,[Val])
evalueOneSem i macroCall mAll = 
    let callName   = fst macroCall
	callArgs   = snd macroCall
	macroDef   = findMacro mAll callName
	macroSem   = semantics macroDef
	macroIface = interface macroDef
	macroArgs  = args macroDef
	links = (valueArgs i macroArgs callArgs)
	in (instanciateSem i macroSem links,
	    instanciateIface i macroIface links,
	    instSemGetParam i macroSem links)


-- from the args of a macro and the args of a call
-- we build a dictionary containing (macro variable, value)
valueArgs :: Int -> FS -> FS -> (Dict.Map String Val)
valueArgs i mArgs cArgs = 
    let errorIfConst y =  
	    case y of Const _ -> error ("macro definition containing constant " ++ (show y)) 
		      _ -> y  
	checkList = map (\(x,y) -> (x, errorIfConst y)) mArgs
	featVar = Dict.fromList checkList
	links = Dict.fromList (map (\(x,y) -> (valToString (find i featVar (Var x)) , y )) cArgs)
 	in links


-- function searching for semantic variables that aren't specified 
-- by the macro call
instSemGetParam :: Int -> Sem -> (Dict.Map String Val) -> [Val]
instSemGetParam i sem args =
    nub $
    filter (\x ->  notElem x (Dict.elems args)) $
    concat $ 
    map (\lit ->
	 let label   = fst3 lit
             pred    = snd3 lit
	     semargs = thd3 lit
	 in 
	 [find i args label,find i args pred]
	 ++(map (\x -> find i args x) semargs)
	) sem


instanciateSem :: Int -> Sem -> (Dict.Map String Val) -> Sem
instanciateSem i sem args = 
    map (\lit -> 
	 let label   = fst3 lit
             pred    = snd3 lit
	     semargs = thd3 lit
	 in (find i args label, find i args pred, map (\x -> find i args x) semargs)) sem
    

instanciateIface :: Int -> FS -> (Dict.Map String Val) -> FS
instanciateIface i fs args =
    map (\(x,y) -> (x, find i args y)) fs
