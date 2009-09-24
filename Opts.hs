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
     , Option ['g']     ["geni"]    (NoArg GeniFormat)    "Output format: geni (UTF8)"
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
  usageInfo header options
 where
  header =
    "lexConverter \n\n"++pname++": lexicon convertion program which "
	     ++"can be used in 2 modes, convertion of lemma lexicons or morphological lexicons.\n\n"
	     ++"Usage: "++pname++" -i file [OPTION...] \n"

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
findLatin1 = elem Recode

findVersion :: String
findVersion = "lexConverter version 2.0 \n\t E. Y. Kow - Y. Parmentier - LORIA 2005 - 2008 \n\n"
