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
