module TraceRetrieving where

import Data.Aeson
import Data.Dynamic
import Data.Map.Strict (Map, insert)
import qualified Data.ByteString.Lazy as B (getContents, split, null, readFile, ByteString, hGetContents)
import qualified Data.ByteString.Internal as BS (c2w)
import qualified Data.Map.Merge.Strict as MM
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)
import Data.Maybe
import System.Process (createProcess, CreateProcess(..), proc, StdStream(CreatePipe))

checkAndConvert :: Map String (Value -> Dynamic) -> Map String Value -> Map String Dynamic
checkAndConvert fromjsoners vals = let
  applyWhenMatched = MM.zipWithMatched (const ($))
  mymerge = MM.merge MM.dropMissing MM.dropMissing applyWhenMatched in
  insert "__rawJSON__" (toDyn vals) $ mymerge fromjsoners vals

getJSONs :: String -> Int -> IO [Map String Value]
getJSONs str howmany = do
  decoded <- getAllJSONs (getRawContent str)
  return $ (if howmany >= 0 then take howmany else id) decoded

getAllJSONs :: IO B.ByteString -> IO [Map String Value]
getAllJSONs rawcontentgetter = do
  content <- rawcontentgetter
  let lines = B.split (BS.c2w '\n') content
  let nonemptylines = filter (not.B.null) lines
  return $ map (fromMaybe (error "No json").decode) nonemptylines

getRawContent :: String -> IO B.ByteString
getRawContent str = do
  mfname <- getMArg "--file"
  let hasfname = isJust mfname
  mprogram <- getMArg "--program"
  let hasprogram = isJust mprogram
  if hasfname then B.readFile (fromJust mfname)
    else if hasprogram then readproc (fromJust mprogram) [str] else
    B.getContents

readproc :: String -> [String] -> IO B.ByteString
readproc program args = do
  (_, outhdl, _, _) <- createProcess (proc program args){ std_out = CreatePipe }
  B.hGetContents (fromJust outhdl)

getMArg :: String -> IO (Maybe String)
getMArg flag = getArgs >>= return.getarg
  where
  getarg (l0:l1:r)
    | l0 == flag = Just l1
    | otherwise = getarg (l1:r)
  getarg _ = Nothing

getJSONsForDyn :: String -> (Map String Value -> Bool) -> Int -> [Map String Value]
getJSONsForDyn x ffun howmany = let
  isinnervalid = unsafePerformIO checkinnervalid
  in
  if not isinnervalid
  then error "No fname for inner spec" else filter ffun $ unsafePerformIO (getJSONs x howmany)

checkinnervalid :: IO Bool
checkinnervalid = do
  hasname <- isJust <$> getMArg "--file"
  hasprogram <- isJust <$> getMArg "--program"
  return (hasname || hasprogram)
