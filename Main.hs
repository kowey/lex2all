module Main (main)

where

-- for command line options and time
import System.Environment (getArgs, getProgName)
import System.Time
import Opts
import System.IO
-- for exiting
import System.Exit (ExitCode(ExitSuccess), exitWith)
import Text.Printf
-- for parsec
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Pos

-- our parsers
import Parser(parserHeader,parserSuite,macros)
import MorphParser(morphParserHeader,morphParserSuite,included)

--our types and functions
import Dtypes
import MacroProcessing
import PrettyPrinting

--misc
import Control.Monad
import qualified Data.Map as Dict
import Control.Exception
import Data.Word (Word8)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray)
import UTF8 (encode, decode)

-- Sources from http://scannedinavian.org/~lemmih/FilePath
import FilePath

main :: IO ()
main = do 
       args  <- getArgs
       pname <- getProgName
       -- result of convertOpts : (option list, non-option list)
       (ol, _) <- converterOpts pname args 
       -- uncomment below to print the content of the command line
       --printList ol
       let needHelp   = findFlag Help ol
       let version    = findFlag Version ol
       if needHelp then do
			hPutStr stderr (help pname)
	                exitWith ExitSuccess 
          else 
            if version then do
                            hPutStr stderr findVersion
	                    exitWith ExitSuccess
            else do
	         let outputfile = findName "output" ol
		     inputfile  = findName "input" ol
		     mode       = findMode ol
	             option     = findFormat ol
		     verbose    = findFlag Verbose ol
                     isLatin1   = findLatin1 ol
		     (file, dir)= case inputfile of  Left  _ -> ("stdin", "")
				                     Right f -> (f, dirname f)
                     realOut    = case outputfile of Left _ -> "stdout"
                                                     Right f -> f
	         time <- getClockTime
                 input <- if (option == "tulipa" && (not isLatin1)) then readUTF8File file
                          else readFile file
	         case mode of 'L' -> convertLex dir verbose pname option file input realOut time 
                              'M' -> convertMorph dir verbose pname option file input realOut time 
                              _   -> error "unknown mode, either -L or -M must be set. Please use option -h."
                 case verbose of True  -> hPutStr stderr "Done\n"
                                 False -> exitWith ExitSuccess
                 exitWith ExitSuccess


-- /******ADDED FOR UTF8 SUPPORT********/
readUTF8File :: FilePath -> IO String
readUTF8File f =
    do fb <- readFileBytes f
       case decode fb of (cs, []) -> return cs
                         (_,  _ ) -> error "unknown encoding, make sure it is UTF8" --fail $ show xs

writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File f cs =
    do fb <- writeFileBytes f $ encode $ cs
       return fb

readFileBytes :: FilePath -> IO [Word8]
readFileBytes f =
    do h <- openBinaryFile f ReadMode
       hsize <- fromIntegral `fmap` hFileSize h
       hGetBytes h hsize

writeFileBytes :: FilePath -> [Word8] -> IO ()
writeFileBytes f ws =
    do h <- openBinaryFile f WriteMode
       hPutBytes h (length ws) ws
       hClose h

hGetBytes :: Handle -> Int -> IO [Word8]
hGetBytes h c = 
    allocaArray c $ \p ->
        do c' <- hGetBuf h p c
           peekArray c' p

hPutBytes :: Handle -> Int -> [Word8] -> IO ()
hPutBytes h c ws = 
    allocaArray c $ \p ->
        do pokeArray p ws
           hPutBuf h p c
-- /*************************************/


toAbsolutePath :: FilePath -> FilePath -> FilePath
toAbsolutePath _ "" = ""
toAbsolutePath parent path = 
    if (head path) == '/' then path else parent ++ "/" ++ path


parseInclude :: Parser ([Char], SourcePos, [String]) -> FilePath -> String -> ([Char], SourcePos, [String])
parseInclude p fileName input = case (parse p fileName input) of
				Left err -> ([],initialPos fileName,[(show err)])
				Right x -> x


parseIncluded :: Parser [a] -> [String] -> [[Char]] -> [a]
parseIncluded p fileNames fileContents =
    let parser pa file content = case (parse pa file content) of
				 Left err ->  error (show err)
				 Right x -> x
	in concat (zipWith (parser p) fileNames fileContents)


parseLexicon :: ([Char] -> SourcePos -> Parser a) -> [Char] -> SourcePos -> FilePath -> a
parseLexicon p suite pos fileName = 
    let parser = (p suite pos) 
    in case (parse parser fileName suite) of
					  Left err -> error (show err) 
					  Right x  -> x


printList :: Show a => [a] -> IO [()]
printList l = mapM (\x -> hPutStr stderr ((show x)++"\n")) l


convertLex :: String -> Bool -> String -> String -> String -> String -> FilePath -> ClockTime -> IO() --Handle -> ClockTime -> IO()
convertLex dir verbose pname option file input ohandle time =
    do 
    let (suite,pos,files) = (parseInclude parserHeader file input)
    contents <- mapM (readFile.(toAbsolutePath dir)) files
    let semMac = (parseIncluded macros files contents)
    --if verbose then printList semMac else hPrintf stderr "%s" ""
    let lexicon = (parseLexicon parserSuite suite pos file)
    --if verbose then printList lexicon else hPrintf stderr "%s" ""
    let realLexicon = if semMac /= [] then (evalueSemLex lexicon (macrosToDict semMac)) 
                      else lexicon
    if verbose then printList realLexicon else hPrintf stderr "%s" ""
    let printers = prettyLPrinters Dict.! option 
	reslex   = ((lheader printers) time pname)++((lcontent printers) realLexicon time file "trees.xml" "lemma.xml" "lexicon.xml")
    --hPutStr ohandle reslex
    if option == "tulipa" then writeUTF8File ohandle reslex
       else
           writeFile ohandle reslex


convertMorph :: String -> Bool -> String -> String -> String -> String -> FilePath -> ClockTime -> IO() --Handle -> ClockTime -> IO ()
convertMorph dir verbose pname option file input ohandle time =
    do
    let (suite,pos,files) = (parseInclude morphParserHeader file input)
    contents <- mapM (readFile.(toAbsolutePath dir)) files
    let inclu = (parseIncluded included files contents)
	lexicon = (parseLexicon morphParserSuite suite pos file)
	realLexicon = (inclu++lexicon)
    if verbose then printList realLexicon else hPrintf stderr "%s" ""
    -- be careful -> option "geni" must not be processed
    if option == "geni" then throwIO (RecSelError "Morphological lexicon -> no printer available yet for the geni format")
       else 
       let printers = prettyMPrinters Dict.! option 
	   reslex   = ((mheader printers) time pname)++((mcontent printers) realLexicon time file "trees.xml" "lemma.xml" "lexicon.xml")
	   in --hPutStr ohandle reslex
             if option == "tulipa" then writeUTF8File ohandle reslex
                else
                    writeFile ohandle reslex

