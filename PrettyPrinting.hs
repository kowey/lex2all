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

