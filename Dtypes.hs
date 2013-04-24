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

module Dtypes (Val(..), valToString, FS, AVPair, Lit, Sem, emptyLit, MacroCall, SemCall, emptyCall, Equa, FeatPath, Coanchor, Lexicon,LexEntry(..), emptyLex, Macros, SemMacro(..), emptySemMacro, XMLelem(..), MorphEntry(..), emptyMorph, Flag(..), HandleOrPath)

where

import Generic
import qualified Data.Map as Dict
import Data.List (intersperse)
import System.IO (FilePath, Handle)

--types definitions

--Values
data Val = Const [String] | Var String | Anonymous deriving (Eq,Ord)

instance Show Val where
    show (Const x)  = concat $ intersperse " ! " $ map toLowerHead x
    show (Var x)    = "?" ++ (toUpperHead x)
    show Anonymous  = "_"

valToString :: Val -> String
valToString x =
    case x of Var a     -> a
              Anonymous -> "_"
              Const [x] -> x
              _         -> error "Not a single value"

-- Feature Structures
type FS   = [AVPair]
type AVPair  = (String,Val)

-- Semantic literals (Label, Predicate, Arguments)
type Lit = (Val, Val, [Val])
type Sem = [Lit]
emptyLit :: Lit
emptyLit = (Anonymous,Anonymous,[])

type MacroCall = (String, FS)
type SemCall = [MacroCall]
emptyCall :: MacroCall
emptyCall = ("",[])

-- Anchoring equations (Node, Feat, Value)
type Equa = (String, FeatPath, Val)
type FeatPath = (String, String)

-- Coanchoring (Node name, lexical item, Cat)
type Coanchor = (String,String,String)

-- lexicon
type Lexicon = Dict.Map String [LexEntry]
-- lexical Entry
data LexEntry = Lex{lemma      :: String,
		    cat        :: String,
		    calls      :: SemCall,
		    params     :: [Val],
                    sem        :: Sem,
		    iface      :: FS,
                    acc        :: String, --unused
                    family     :: String,
                    filters    :: FS,
		    except     :: [String], --to be checked
		    equations  :: [Equa],
		    coanchors  :: [Coanchor]}
		deriving (Show, Eq)

instance Ord LexEntry where
    compare a b = compare (lemma a) (lemma b)

emptyLex :: LexEntry
emptyLex = Lex { lemma     = "",
                 cat       = "",
		 calls     = [],
		 params    = [],
                 sem       = [],
		 iface     = [],
                 acc       = "",
		 family    = "",
                 filters   = [],
                 except    = [],
                 equations = [],
                 coanchors = [] }

-- semantic macros
type Macros = Dict.Map String SemMacro
data SemMacro = Macro { name      :: String,
			args      :: FS,
			semantics :: Sem,
			interface :: FS }
		deriving (Show, Eq)


instance Ord SemMacro where
    (<) a b = (name a) < (name b)

emptySemMacro :: SemMacro
emptySemMacro = Macro { name      = "",
			args      = [],
			semantics = [],
			interface = [] }

-- XML element (ie tag) with attributes (ie features),
-- potential datas and sub-XML elements (ie children)
data XMLelem = Elem {
		  tag      :: String,
		  features :: [(String,String)],
		  datas    :: String,
		  children :: [XMLelem] }
	     | EmptyElem
	       deriving (Eq, Show)

-- Morphological Entry
data MorphEntry = Morph {
			 morph      :: String,
			 lem        :: String,
			 mcat       :: String,
			 morphFeats :: FS }
		  deriving (Eq, Show)

emptyMorph :: MorphEntry
emptyMorph = Morph {
		    morph      = "",
		    lem        = "",
		    mcat       = "",
		    morphFeats = []}

instance Ord MorphEntry where
    compare a b = compare (morph a) (morph b)

-- processing of the command line
data Flag
    = Verbose  | Help | Version | MorphMode | LexMode | Recode | DyalogFormat
    | LLP2Format | GeniFormat | TTMCTAGFormat | XMLFormat | Input String | Output String
    deriving (Eq, Show)

type HandleOrPath = Either Handle FilePath
