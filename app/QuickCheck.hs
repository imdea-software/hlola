module QuickCheck where
import Test.QuickCheck
import Lola
import DecDyn
import Data.Map.Strict ((!), singleton)
import Data.Dynamic
import InFromFile
import Lib.LTL
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Maybe
import Data.List (findIndex)

main = quickCheck historically_is_correct

historically_is_correct :: [Bool] -> Bool
historically_is_correct bs = let
    thetrace = map (unpack . encode . singleton "p") bs
    thejsons = lines $ runSpecJSON False myspec $ unlines thetrace
    themaps = map (fromJust.decode.pack) thejsons
    mMinNotP = findIndex not $ map (\m -> m ! "p") themaps
    mMaxHistP = findLastIndex id $ map (\m -> m ! "historically<p>") themaps
    minNotP = fromMaybe (length themaps) mMinNotP
    maxHistP = fromMaybe (-1) mMaxHistP
  in
    minNotP - 1 == maxHistP
  where
    findLastIndex f l = findIndex f (reverse l) >>= \a -> Just (length l - a-1)

myspec :: Specification
myspec = [out $ historically p, out p]

p :: Stream Bool
p = Input "p"
