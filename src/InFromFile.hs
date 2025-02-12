module InFromFile where

import Lola
import Engine.Engine
import StaticAnalysis
import Data.Dynamic
import Data.Map.Strict (Map,fromList,(!?), notMember)
import Data.CSV
import Text.ParserCombinators.Parsec
import Data.Either
import Data.Maybe
import DecDyn
import System.Exit
import System.IO (hPutStr, stderr)
import Data.List
import TraceRetrieving

(!!!) :: Map String y -> String -> y
x !!! y = fromMaybe (error ("Not referenced input: " ++ y)) $ x !? y

getInstants :: Readers -> [String] -> [String] -> Map Ident Dynamic
getInstants readers tags vals = let
  tagvals = zip tags vals
  dafun (t,v) = let
    mreader = readers !? t
    in maybe Nothing (\rdr -> Just (t, rdr v)) mreader
  thelist = map dafun tagvals in
  fromList (catMaybes thelist)

getNonReferredInputs :: Readers -> [String] -> [String]
getNonReferredInputs readers tags = 
  filter (flip notMember readers) tags

runSpecJSON :: Bool -> Specification -> IO ()
runSpecJSON = runSpecJSONWithPast []

runSpecJSONWithPast :: [Map Ident Dynamic] -> Bool -> Specification -> IO ()
runSpecJSONWithPast pastlist debug decs = do
  jsons <- getJSONs "ROOT" (-1)
  let instants = map (checkAndConvert (getFromJSONers decs)) jsons
  putStrLn (runWithPast pastlist JSON debug decs instants)

runSpecCSV :: Bool -> Specification -> IO ()
runSpecCSV debug decs =
  do
  content <- getContents
  let csvs = map (++"\n") $ lines content
      (hd:instants) = concatMap (fromRight (error "No Right").parse csvFile "(stdin)") csvs
      readers = getReaders decs
      nri = getNonReferredInputs readers hd
      decodedinstants = map (getInstants readers hd) instants
      outstderr = if null nri then "" else "Warning, non referenced inputs: " ++ show nri ++ "\n"
  hPutStr stderr outstderr
  putStrLn $ intercalate "," $ map (dgetId.fst4) decs
  putStrLn $ run CSV debug decs decodedinstants

analyse :: Specification -> IO ()
analyse decs = let
  -- g is the dependency graph of the specification
  g = getFGraph (map fst4 decs)
  -- dot is the dotfile to plot the dependency graph
  dot = dotFromGraph g
  showdot = "Dot file:\n" ++ dot in do
  putStrLn showdot
  case checkGraph g of
      Right _ -> exitSuccess
      Left err -> putStrLn err >> exitWith (ExitFailure 1)
