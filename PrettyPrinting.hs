module PrettyPrinting (LPrettyPrinter(..),MPrettyPrinter(..), prettyLPrinters, prettyMPrinters)

where

import Dtypes
import System.Time
import qualified Data.Map as Dict

import LexToDyalog
import LexToLLP2
import LexToGeni
import LexToGeniXML
import LexToTTMCTAG
import MorphToDyalog
import MorphToLLP2
import MorphToTTMCTAG

-- data types for pretty printing
data LPrettyPrinter = LPrint { lheader  ::  ClockTime -> String -> String,
			       lcontent :: [LexEntry] -> ClockTime -> String -> String -> String -> String -> String }

data MPrettyPrinter = MPrint { mheader  ::  ClockTime -> String -> String,
			       mcontent :: [MorphEntry] -> ClockTime -> String -> String -> String -> String -> String }

type LPrinters = Dict.Map String LPrettyPrinter
type MPrinters = Dict.Map String MPrettyPrinter

-- dictionaries containing the name of the functions
prettyLPrinters :: LPrinters
prettyLPrinters = 
    Dict.fromList 
	    [("dyalog",LPrint {
			       lheader  = headerDya,
			       lcontent = formatLexDya }),
	     ("llp2",LPrint {
			     lheader  = headerLLP,
			     lcontent = formatLexLLP }),
	     ("geni",LPrint {
			     lheader  = headerGen,
     	                     lcontent = formatLexGen }),
	     ("tulipa",LPrint {
			         lheader  = headerTT,
			         lcontent = formatLexTT }),
	     ("xml",LPrint {
			    lheader  = headerGenXML,
			    lcontent = formatLexGenXML })
            ]


prettyMPrinters :: MPrinters
prettyMPrinters = 
    Dict.fromList 
	    [("dyalog",MPrint {
			       mheader  = morphHeaderDya,
			       mcontent = formatMorphDya }),
	     ("llp2",MPrint {
			     mheader  = morphHeaderLLP,
			     mcontent = formatMorphLLP }),
             ("tulipa",MPrint {
			       mheader  = headerTT,
			       mcontent = formatMorphTT })
            ]

