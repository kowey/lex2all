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

module Main (main)

where

-- for command line options and time
import System.Environment (getArgs, getEnv, getProgName)
import System.Time
import Opts
import System.IO
-- for exiting
import System.Exit (exitSuccess)
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
import qualified System.IO.UTF8 as U

import System.FilePath ( takeDirectory, isAbsolute )

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
                        exitSuccess
          else
            if version then do
                            hPutStr stderr findVersion
                            exitSuccess
            else do
                 curdir <- getEnv "PWD"
                 let outputfile  = findName "output" ol
                     inputfile   = findName "input" ol
                     mode        = findMode ol
                     option      = findFormat ol
                     verbose     = findFlag Verbose ol
                     recodeLatin1= findLatin1 ol
                     (file, dir) = case inputfile of  Left  _ -> ("stdin", "")
                                                      Right f -> (f, takeDirectory (toAbsolutePath curdir f))
                     realOut     = case outputfile of Left _ -> "stdout"
                                                      Right f -> f
                 time <- getClockTime
                 input <- if option == "tulipa" && not recodeLatin1 then U.readFile file
                          else readFile file
                 case mode of 'L' -> convertLex dir verbose pname option file input realOut time
                              'M' -> convertMorph dir verbose pname option file input realOut time
                              _   -> error "unknown mode, either -L or -M must be set. Please use option -h."
                 when verbose $ hPutStrLn stderr "Done"
                 exitSuccess


toAbsolutePath :: FilePath -> FilePath -> FilePath
toAbsolutePath _ "" = ""
toAbsolutePath parent path =
    if isAbsolute path then path else parent ++ "/" ++ path


parseInclude :: Parser ([Char], SourcePos, [String]) -> FilePath -> String -> ([Char], SourcePos, [String])
parseInclude p fileName input = case (parse p fileName input) of
                                Left err -> ([],initialPos fileName,[show err])
                                Right x -> x


parseIncluded :: Parser [a] -> [String] -> [[Char]] -> [a]
parseIncluded p fileNames fileContents =
    let parser pa file content = case parse pa file content of
                                 Left err ->  error (show err)
                                 Right x -> x
        in concat (zipWith (parser p) fileNames fileContents)


parseLexicon :: ([Char] -> SourcePos -> Parser a) -> [Char] -> SourcePos -> FilePath -> a
parseLexicon p suite pos fileName =
    let parser = p suite pos
    in case (parse parser fileName suite) of
       Left err -> error (show err)
       Right x  -> x


printList :: Show a => [a] -> IO ()
printList = mapM_ (hPrint stderr)


convertLex :: String -> Bool -> String -> String -> String -> String -> FilePath -> ClockTime -> IO() --Handle -> ClockTime -> IO()
convertLex dir verbose pname option file input ohandle time = do
    let (suite, pos, files) = parseInclude parserHeader file input
    contents <- mapM (readFile . toAbsolutePath dir) files
    let semMac = (parseIncluded macros files contents)
    --if verbose then printList semMac else hPrintf stderr "%s" ""
    let lexicon = (parseLexicon parserSuite suite pos file)
    --if verbose then printList lexicon else hPrintf stderr "%s" ""
    let realLexicon = if semMac /= [] then evalueSemLex lexicon (macrosToDict semMac)
                      else lexicon
    if verbose then printList realLexicon else hPrintf stderr "%s" ""
    let printers = prettyLPrinters Dict.! option
        reslex   = lheader printers time pname ++ lcontent printers realLexicon time file "trees.xml" "lemma.xml" "lexicon.xml"
    --hPutStr ohandle reslex
    writeFileFor option ohandle reslex


convertMorph :: String -> Bool -> String -> String -> String -> String -> FilePath -> ClockTime -> IO() --Handle -> ClockTime -> IO ()
convertMorph dir verbose pname option file input ohandle time = do
    let (suite, pos, files) = parseInclude morphParserHeader file input
    contents <- mapM (readFile . toAbsolutePath dir) files
    let inclu = (parseIncluded included files contents)
        lexicon = (parseLexicon morphParserSuite suite pos file)
        realLexicon = (inclu++lexicon)
    if verbose then printList realLexicon else hPrintf stderr "%s" ""
    -- be careful -> option "geni" must not be processed
    if option == "geni" then throwIO (RecSelError "Morphological lexicon -> no printer available yet for the geni format")
       else
       let printers = prettyMPrinters Dict.! option
           reslex   = mheader printers time pname ++ mcontent printers realLexicon time file "trees.xml" "lemma.xml" "lexicon.xml"
           in --hPutStr ohandle reslex
              writeFileFor option ohandle reslex

writeFileFor :: String   -- ^ format
             -> FilePath -- ^ file
             -> String   -- ^ contents
             -> IO ()
writeFileFor "geni"   = U.writeFile
writeFileFor "tulipa" = U.writeFile
writeFileFor _ = writeFile
