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

module MorphParser (morphParserHeader,morphParserSuite,included)

where

-- parser and scanner definitions
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Token (TokenParser, LanguageDef(..), makeTokenParser)
import qualified Text.ParserCombinators.Parsec.Token as P

import Text.ParserCombinators.Parsec.Pos (SourcePos)
import Text.ParserCombinators.Parsec.Prim (getPosition, setPosition)

-- types are needed
import Dtypes

lexer :: TokenParser ()
lexer  = makeTokenParser
         (emptyDef
	  { commentLine = "%"
          , commentStart = "/*" 
          , commentEnd = "*/"
	  , identStart = noneOf " \v\f\t\r\n*,.!?;:[]()|<>/%=" --alphaNum <|> oneOf "+-_äüöß=#^{"
	  , identLetter = noneOf " \v\f\t\r\n*,.!?;:[]()|<>/%=" --alphaNum <|> oneOf "-_'+.äüöß=#^{}"
	  , nestedComments = True 
	  , caseSensitive = True
	  , reservedNames = ["include"]
          })

whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
squares   = P.squares lexer
symbol    = P.symbol  lexer
semi      = P.semi    lexer
reserved  = P.reserved lexer


morphParserHeader :: Parser ([Char], SourcePos, [String])
morphParserHeader = do
                    whiteSpace
		    m <- many inclusion
		    pos <- getPosition
		    suite <- getInput
		    return (suite,pos,m)

morphParserSuite :: [Char] -> SourcePos -> Parser [MorphEntry]
morphParserSuite suite pos = do
			     setPosition pos
			     setInput suite
			     l <- many entry
                             whiteSpace
			     eof
			     return l

inclusion :: Parser String
inclusion = do
	    reserved "include"
	    name <- identifier <?> "file name"
	    return name

included :: Parser [MorphEntry]
included = do 
	   e <- many entry
	   eof
	   return e

entry :: Parser MorphEntry
entry = do 
	morph <- identifier <?> "morphological item"
	spaces
	lemma <- identifier <?> "lemma"
	spaces
	feats <- morphofeats <?> "morphological features (AVM)"
	option ' ' newline
	let cat = getCat feats 
	    mfeats = getFeats feats
	return Morph{morph      = morph,
		     lem        = lemma, 
		     mcat       = cat, 
		     morphFeats = mfeats
		    }


morphofeats :: Parser FS
morphofeats = option [] $ squares $ sepEndBy getAttVal semi

getAttVal :: Parser AVPair
getAttVal = do
	    att <- identifier <?> "attribute" 
	    symbol "="
	    whiteSpace
	    val <- atomicDisj <|> value <?> "feature value" 
	    whiteSpace
	    return (att, val)

-- variables have to begin with "?"
value :: Parser Val
value = do
	p <- option " " (symbol "?")
	h <- identifier <?> "identifier"
	let val = if (head p) == '?' then Var (h)
		  else Const (h:[])
	return val

atomicDisj :: Parser Val
atomicDisj = do 
	     values <- option [] $ (identifier <?> "identifier") `sepBy1` (symbol "|")
	     return (Const values)

getCat :: FS -> String
getCat fs =
    let l = filter (\(x,_) -> x=="pos") fs
	in if null l then ""
	   else fromVal $ snd $ head l

getFeats :: FS -> FS
getFeats fs =
    filter (\(x,_) -> x /= "pos") fs

fromVal :: Val -> String
fromVal v =
    case v of Const [x] -> x
	      Var x -> x
	      _ -> error "Non unique const value"
