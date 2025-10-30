{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Network2 where
import Data.Aeson
import DecDyn
import GHC.Generics
import Lola
import Syntax.Booleans
import Syntax.HLPrelude hiding (error)
import Syntax.Num
import Syntax.Ord
import Lib.DynPar
import qualified Data.Set as Set
import qualified Prelude as P
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe

spec :: Specification
spec = [out sensors, out all_ok, out sensorData]

sensorId :: Stream Int
sensorId = Input "sensorId"

sensorData :: Stream Int
sensorData = Input "sensorData"

sensors :: Stream (Set.Set Int)
sensors = "sensors" =:
  Set.insert <$> Now sensorId <*> sensors :@ (-1, Leaf Set.empty)

sensor :: Int -> Stream Int
sensor n = "sensor" <: n =: Now sensorData

windowSum :: Int -> Stream Int
windowSum n = "windowSum" <: n =:
  windowSum n :@ (-1,0) + Now (sensor n) - sensor n :@ (-10,0)

-- This average is wrong here and in the original spec
average :: Int -> Stream Int
average n = "average" <: n =:
  div <$> Now (windowSum n) <*> 10

threshold1 = 500
threshold2 = 1000
threshold3 = 3

semiDanger :: Int -> Stream Bool
semiDanger n = "semiDanger" <:n =:
  Now (average n) > threshold1

danger :: Int -> Stream Bool
danger n = "danger" <:n =:
  Now (average n) > threshold2

all_ok :: Stream Bool
all_ok = "all_ok" =: let
  nsemidangers = Map.size.Map.filter id <$> semiDanger `over` sensors -- `when` myfilter
  dangerset = Map.filter id <$> danger `over` sensors -- `when` myfilter
  in
  nsemidangers < threshold3
  && Map.null <$> dangerset

myfilter p m = p == extract sensorId m

-- Original specification (from https://www.react.uni-saarland.de/publications/Rosenhauer19.pdf , Fig 4.2):
-- What if the average is exactly threshold? It wouldn't be terminated..
--
-- input int SensorId
-- input int SensorData
-- 
-- output bool action <int id >
-- invoke : SensorId
-- := SensorId = id
-- 
-- output int splitData <int id >
-- invoke : SensorId
-- extend : action
-- := SensorData
-- 
-- output int windowSum <int id >
-- invoke : SensorId
-- extend : action
-- := ( windowSum ( id ) [ -1 ,0] + splitData ( id ) [0 ,0]) -
-- splitData ( id ) [ -10 ,0]
-- 
-- output int average <int id >
-- invoke : SensorId
-- extend : action
-- := windowSum ( id ) [0 ,0] / 10
-- 
-- output bool highValue <int id >
-- invoke : SensorId
-- := average ( id ) [0 ,0] > threshold1
-- 
-- output int newAlert <int id >
-- invoke : SensorId
-- extend : highValue
-- := id
-- 
-- output bool terminAlert <int id >
-- invoke : newAlert
-- terminate : terminAlert
-- := average ( id ) [0 ,0] < threshold1
-- 
-- output bool Alert <int id >
-- invoke : newAlert
-- terminate : terminAlert
-- := average ( id ) [0 ,0] > threshold2
-- 
-- trigger any ( Alert = true )
-- trigger count ( Alert ) > threshold3
