module Opts (converterOpts,findFlag,help,findMode,findFormat,findName,findLatin1,findVersion)

where
    
import System.Console.GetOpt
import Data.Maybe ( catMaybes )
import System.IO
-- for type Flag
import Dtypes
    
options :: [OptDescr Flag]
options =
     [ Option ['v']     ["verbose"] (NoArg Verbose)       "verbose output on stderr"
     , Option ['w']     ["which"]   (NoArg Version)       "version number"
     , Option ['H','h'] ["help"]    (NoArg Help)          "show help"
     , Option ['L']     ["lemmas"]  (NoArg LexMode)       "Converting mode -> lemmas"
     , Option ['M']     ["morph"]   (NoArg MorphMode)     "Converting mode -> morphological items"
     , Option ['r']     ["recode"]  (NoArg Recode)        "Define input encoding as being Latin1 (tulipa only)"
     , Option ['d']     ["dyalog"]  (NoArg DyalogFormat)  "Output format: dyalog (Latin1)"
     , Option ['l']     ["llp2"]    (NoArg LLP2Format)    "Output format: llp2 (Latin1)"
     , Option ['g']     ["geni"]    (NoArg GeniFormat)    "Output format: geni (Latin1)"
     , Option ['x']     ["xml"]     (NoArg XMLFormat)     "Output format: XML (Latin1)"
     , Option ['t']     ["tulipa"]  (NoArg TTMCTAGFormat) "Output format: tulipa (UTF8)"
     , Option ['o']     ["output"]  (ReqArg Output "FILE")"Output FILE (default: stdout)"
     , Option ['i']     ["input"]   (ReqArg Input "FILE") "Input FILE (default: stdin)"
     ]
    
    
converterOpts :: String -> [String] -> IO ([Flag], [String])
converterOpts progname argv = 
    case getOpt Permute options argv of
				     (o,n,[]  ) -> return (o,n)
				     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
				     where header = "Usage: "++ progname ++" [OPTION...] files..."


help :: String -> String
help pname = 
    "lexConverter \n\n"++pname++": lexicon convertion program which "
	     ++"can be used in 2 modes, convertion of lemma lexicons or morphological lexicons.\n\n"
	     ++"Usage: "++pname++" -i file [OPTION...] \n"
	     ++"\t-v        --verbose        verbose output on stderr\n"
	     ++"\t-w        --which          version number\n"
	     ++"\t-H, -h    --help           show help\n"
	     ++"\t-L        --lemmas         Converting mode -> lemmas\n"
	     ++"\t-M        --morphs         Converting mode -> morphological items\n"
             ++"\t-r        --recode         Define input encoding as being Latin1 (tulipa only)\n" 
	     ++"\t-d        --dyalog         Output format: dyalog (Latin1)\n"
	     ++"\t-l        --llp2           Output format: llp2 (latin1)\n"
	     ++"\t-g        --geni           Output format: geni (Latin1)\n"
	     ++"\t-x        --xml            Output format: XML (Latin1)\n"
	     ++"\t-t        --tulipa         Output format: tulipa (UTF8)\n"
	     ++"\t-o [FILE] --output[=FILE]  output FILE (default: stdout)\n"
	     ++"\t-i [FILE] --input[=FILE]   input FILE (default: stdin)\n\n"


findFlag :: Flag -> [Flag] -> Bool
findFlag f ol = elem f ol


findMode :: [Flag] -> Char
findMode ol = 
    let mode = filter (\x -> x == LexMode || x== MorphMode) ol
	in if (length mode) /= 1 then error "Converting mode unknown, -(L|M) must be set. Please type -h (note that only one mode is allowed)"
	   else let m = (head mode)  
		    in case m of LexMode   -> 'L'
				 MorphMode -> 'M'
				 _         -> error "error in findMode -(L|M)"


findFormat :: [Flag] -> String
findFormat ol =
    let format = filter (\x -> x == DyalogFormat || x== LLP2Format || x== GeniFormat || x== TTMCTAGFormat || x== XMLFormat) ol
	in if (length format) /= 1 then error "Converting output format unknown, please type -h (note that only one output format is allowed)"
	   else let f = (head format)  
		    in case f of DyalogFormat -> "dyalog"
				 LLP2Format   -> "llp2"
				 GeniFormat   -> "geni"
				 TTMCTAGFormat-> "tulipa"
                                 XMLFormat    -> "xml"
				 _            -> error "error in findFormat -(d|l|g|t|x)"


findName :: String -> [Flag] -> HandleOrPath
findName x ol =
    let fromOutput (Output o) = Just o
	fromOutput _          = Nothing
	fromInput  (Input i)  = Just i
	fromInput  _          = Nothing
	(filterFn, default_)  =
	    case x of "output" -> (fromOutput, Left stdout)
		      "input"  -> (fromInput,  Left stdin)
		      _        -> error "error in findName -(o|i)"
	in case (catMaybes $ map filterFn ol) of
					      []  -> default_
					      [a] -> Right a
					      _   -> error ("Too many " ++ x ++ " files. Please type -h (note that only one " ++ x ++ " file is allowed.")


findLatin1 :: [Flag] -> Bool
findLatin1 ol =
    let encoding = filter (\x -> x == Recode) ol 
        in if (length encoding) == 1 then True
           else False


findVersion :: String
findVersion = "lexConverter version 2.0 \n\t E. Y. Kow - Y. Parmentier - LORIA 2005 - 2008 \n\n"
