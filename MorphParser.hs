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
	   else fromVal $ snd $ head $ l

getFeats :: FS -> FS
getFeats fs =
    filter (\(x,_) -> x /= "pos") fs

fromVal :: Val -> String
fromVal v =
    case v of Const [x] -> x
	      Var x -> x
	      _ -> error "Non unique const value"
