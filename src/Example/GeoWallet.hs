{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.GeoWallet where
import Data.Time
import DecDyn
import Lola
import Syntax.Booleans
import Syntax.HLPrelude
import Syntax.Num
import Syntax.Ord
import qualified Data.Map.Strict as Map

type Addr = String
type TimeStamp = String
type Buckets = Map.Map Int Int
emptyBucket = Map.fromList [(h,0) | h <-[0..23]]

spec :: Addr -> Specification
spec addr = [out (buckets addr)]

--- Query: https://api.tzkt.io/v1/accounts/[addr]/operations
-- Extract from path ".initiator.address"
initiator :: Stream Addr
initiator = Input "initiator"

-- Extract from path ".timestamp"
timestamp :: Stream TimeStamp
timestamp = Input "timestamp"

buckets :: Addr -> Stream Buckets
buckets addr = "buckets" =: let
  thisbucket = bucketfromtime <$> Now timestamp
  oldbucket = buckets addr :@ (-1, Leaf emptyBucket)
  in if Now initiator /== Leaf addr
  then oldbucket
  else Map.adjust (+1) <$> thisbucket <*> oldbucket

bucketfromtime :: TimeStamp -> Int
bucketfromtime ts = let
  utctime = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" ts
  in todHour.timeToTimeOfDay.utctDayTime $ utctime

