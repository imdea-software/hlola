module InFromFile where

import Data.Aeson
import Lola
import Engine.Engine
import Data.Dynamic
import qualified Data.Map.Strict as Map
import Data.Scientific
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Internal as BS (c2w)
import Data.CSV
import Text.ParserCombinators.Parsec
import Data.Either
import DecDyn
import qualified Data.Map.Merge.Strict as MM

checkAndConvert :: Map.Map Ident (Value -> Dynamic) -> Map.Map Ident Value -> Map.Map Ident Dynamic
checkAndConvert fromjsoners vals = let
  applyWhenMatched = MM.zipWithMatched (const ($))
  --errMissing = MM.mapMissing (\k _ -> error $ "Missing jsoner for "++(T.unpack k))
  --mymerge = MM.merge MM.dropMissing errMissing applyWhenMatched in
  mymerge = MM.merge MM.dropMissing MM.dropMissing applyWhenMatched in
  mymerge fromjsoners vals

getInstants :: Readers -> [String] -> [String] -> Map.Map Ident Dynamic
getInstants readers tags vals = let
  tagvals = zip tags vals
  thelist = map (\(t,v) -> (t,readers Map.! t $ v)) tagvals in
  Map.fromList thelist

runSpecJSON :: Bool -> Specification -> String -> String
runSpecJSON debug decs cont = let
  content = BC.pack cont
  jsons = B.split (BS.c2w '\n') content
  instants = map ((maybe (error "No json") (checkAndConvert (getFromJSONers decs))).decode) (filter (not.B.null) jsons)
  in
  run JSON debug decs instants

runSpecCSV :: Bool -> Specification -> String -> String
runSpecCSV debug decs content =
  let csvs = map (++"\n") $ lines content
      (hd:instants) = concatMap ( fromRight (error "No Right") . (parse csvFile "(stdin)")) csvs
      decodedinstants = map (getInstants (getReaders decs) hd) instants
    in run CSV debug decs decodedinstants
