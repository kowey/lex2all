module Parser (parserHeader,parserSuite,macros)

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
--import Debug.Trace

lexer :: TokenParser ()
lexer  = makeTokenParser
         (emptyDef
	  { commentLine = "%"
          , commentStart = "/*" 
          , commentEnd = "*/"
	  , identStart = noneOf " \v\f\t\r\n*,.!?;:[]()|<>/%=" --alphaNum <|> oneOf "+-_äüö{"
	  , identLetter = noneOf " \v\f\t\r\n*,.!?;:[]()|<>/%=" --alphaNum <|> oneOf "-_'+äüö{}" 
	  , nestedComments = True 
	  , caseSensitive = True
	  , reservedNames = ["include","semantics","interface"]
          })

-- Should "." be part of identLetter ? it was used to deal with file names

whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
natural   = P.natural lexer
squares   = P.squares lexer
symbol    = P.symbol  lexer
comma     = P.comma   lexer
colon     = P.colon   lexer
braces    = P.braces  lexer
reserved  = P.reserved lexer
parens    = P.parens lexer

parserHeader :: Parser ([Char], SourcePos, [String])
parserHeader = do
	       m <- many inclusion
	       pos <- getPosition
	       suite <- getInput
	       return (suite,pos,m)

parserSuite :: [Char] -> SourcePos -> Parser [LexEntry]
parserSuite suite pos = do
			setPosition pos
			setInput suite
			l <- many entry
                        whiteSpace
			eof
			return l

inclusion :: Parser String
inclusion = do
	    reserved "include"
	    rootname <- identifier <?> "file name (without extension)"
            char '.'
            extension <- identifier <?> "file extension"
	    return (rootname++"."++extension)

macros :: Parser [SemMacro]
macros = do 
	 m <- many macro
	 eof
	 return m

macro :: Parser SemMacro
macro = do
	name <- identifier <?> "macro name"
	args <- feats <?> "macro arguments"
	whiteSpace
	reserved "semantics"
	symbol ":"
	semantics <- squares (option [] $ sepEndBy getLit whiteSpace) <?> "semantic literals"
	whiteSpace
	reserved "interface"
	symbol ":"
	interface <- feats <?> "interface avm"
	return Macro {
		      name = name,
		      args = args,
		      semantics = semantics,
		      interface = interface
		     }

getLit :: Parser Lit
getLit = do
	 label <- value <?> "label"
	 x <- option (Anonymous,[]) getLiteral 
	 let p = fst x
	     a = snd x
	 return (label, p ,a) 

getLiteral :: Parser (Val, [Val])
getLiteral = do 
	     symbol ":" 
	     pred  <- value <?> "predicate"
	     args  <- parens (option [] $ sepBy value comma) <?> "arguments" 
	     let lit = (pred, args)
	     return lit

entry :: Parser LexEntry
entry = do 
	let key k = do { symbol ("*"++k)
		       ; optional space; char ':'; optional spaces }
	whiteSpace
	key "ENTRY"; lex <- identifier <?> "lemma"
	key "CAT"; cat <- identifier <?> "category"
	key "SEM"; sem <- option [] semParser <?> "semantic macro instanciation"
        optional lambda
	key "ACC"; optional natural <?> "acception" --ignored
	key "FAM"; fam <- identifier <?> "family name"
	key "FILTERS"; filter <- option [] filParser <?> "filters"
	key "EX"; option [] (braces (many $ noneOf "{}")) <?> "exceptions" --ignored
	key "EQUATIONS"; optional newline 
        equations <- option [] getEquations <?> "equations"
	key "COANCHORS"; optional newline
        coanchors <- option [] getCoanchors <?> "coanchors"
	return Lex{
		   lemma = lex, 
		   cat = cat, 
		   calls = sem,
		   params = [],
		   sem = (convertSem sem),
		   iface = [],
		   acc = "", 
		   family = fam, 
		   filters = filter, 
		   except = [], 
		   equations = equations, 
		   coanchors = coanchors
		   }

lambda :: Parser ()
lambda = try (
              do
              symbol "*LAM"
              colon
              optional spaces
              optional (many (noneOf "*")) <?> "lambda term" --ignored
             )

semParser :: Parser SemCall
semParser = do
	    s <- many semCall
	    return s

semCall :: Parser MacroCall
semCall = do
	  macro <- identifier <?> "macro name"
	  avm <- feats <?> "macro arguments (avm)"
	  return (macro,avm)

convertSem :: SemCall -> Sem
convertSem semcall =
    (map (\x -> convertLit x) semcall)

convertLit :: MacroCall -> Lit
convertLit macro =
    (Anonymous, Const [(fst macro)], map (\x -> (snd x)) (snd macro))

-- semParseInfo :: Parser Sem
-- semParseInfo = do
--                s <- many semLit
--                return s

-- semLit :: Parser [Lit]
-- semLit = do
-- 	  pred <- identifier <?> "pred name"
-- 	  avm <- feats <?> "pred arguments (avm)"
-- 	  return (Anonymous, pred, avm)

filParser :: Parser FS
filParser = do 
	    fil <- feats <?> "filtering avm" 
	    return fil

getEquations :: Parser [Equa]
getEquations = sepBy getEquation whiteSpace

getEquation :: Parser Equa
getEquation = do 
	      node <- identifier <?> "node name"
	      symbol "->" <?> "equation"
	      feat <- getPath <?> "feature path"
	      symbol "="
	      val <-  atomicDisj <|> value <?> "feature value"
	      whiteSpace
	      return (node, feat, val) 

getPath :: Parser FeatPath
getPath = do
          fs <- identifier
          option "" $ symbol "."
          sn <- option "" $ identifier 
          case sn of "" -> return (sn, fs)
                     _  -> return (fs, sn)

getCoanchors :: Parser [Coanchor]
getCoanchors = sepBy getCoanchor whiteSpace

getCoanchor :: Parser Coanchor
getCoanchor = do 
	      node <- identifier <?> "node name"
	      symbol "->" <?> "coanchor"
	      lex <- identifier <?> "lexical item"
	      symbol "/"
	      cat <- identifier <?> "coanchor category"
	      return (node, lex, cat) 

feats :: Parser FS
feats = option [] $ squares $ sepBy attVal comma

attVal :: Parser AVPair
attVal = do
	 att <- identifier <?> "attribute" 
	 symbol "="
	 whiteSpace
	 val <- atomicDisj <|> value <?> "feature value"
	 whiteSpace
	 return (att,val) 


-- variables have to begin with "?"
value :: Parser Val
value = do
	p <- option ' ' (oneOf "?!")
	h <- identifier <?> "identifier"
	let val = if (p) /= '?' then Const ([h])
		  else Var (h) 
	return val

atomicDisj :: Parser Val
atomicDisj = do 
	     values <- identifier `sepBy1` (symbol "|")
	     return (Const values)
