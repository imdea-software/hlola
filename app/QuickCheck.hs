module QuickCheck where
import Test.QuickCheck
import Lola
import DecDyn
import Data.Map.Strict ((!), singleton)
import Data.Dynamic
import Lib.LTL
import Data.Maybe
import Data.List (findIndex)
import Engine.Engine

main :: IO ()
main = quickCheck historicallyIsCorrect

historicallyIsCorrect :: [Bool] -> Bool
historicallyIsCorrect bs = let
    p = Input "p"
    spec = [out $ historically p, out p]
    trace = runLib spec (map (singleton "p".toDyn) bs)
    mMinNotP = findIndex not $ map (retrieve "p") trace
    retrieve strid m = fromJust.fromDynamic $ m ! strid
    mMaxHistP = findLastIndex id $ map (retrieve "historically<p>") trace
    minNotP = fromMaybe (length trace) mMinNotP
    maxHistP = fromMaybe (-1) mMaxHistP
  in minNotP - 1 == maxHistP
  where
    findLastIndex f l = findIndex f (reverse l) >>= \a -> Just (length l - a-1)
