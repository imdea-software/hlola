{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib.SimuSpec where
import Syntax.HLPrelude
import Data.Aeson
import Data.Either
import GHC.Generics
import Lola
import Engine.Engine
import DecDyn
import Syntax.Ord
import Syntax.Num
import Lib.MTL
import Lib.LTL
import Syntax.Booleans
import Lib.Utils
import Theories.Geometry2D
import Theories.GuidanceTheory
import qualified Data.Map.Strict as Map
import qualified Prelude as P
import Data.Maybe
import Data.List
import Data.Dynamic
import Lola
import Lib.InnerSpec

simuspec :: NonLinearData -> Double -> Double -> Specification
simuspec nld tau maxtime = [out reach_pos, out reached_maxtime, out reach_roll, out reach_distance, out reach_time]
  where
  route = Input "route" :: Declaration (Point2, Double, Maybe Point2)
  reaching_state = get_reaching_state nld tau maxtime <$> pos <*> yaw <*> roll <*> time <*> distance <*> Now route
  ----
  reached_maxtime = "reached_maxtime" =: valstream "reached_maxtime" <$> reaching_state :: Declaration Bool
  time = reach_time :@ (-1, 0)
  reach_time = "reach_time" =: valstream "time" <$> reaching_state
  distance = reach_distance :@ (-1, 0)
  reach_distance = "reach_distance" =: valstream "distance" <$> reaching_state
  yaw = reach_yaw :@ (-1, Leaf$initial_yaw nld)
  reach_yaw = "reach_yaw" =: valstream "yaw" <$> reaching_state
  roll = reach_roll :@ (-1, Leaf$initial_roll nld)
  reach_roll = "reach_roll" =: valstream "roll" <$> reaching_state
  pos = reach_pos :@ (-1, Leaf$initial_pos nld)
  reach_pos = "reach_pos" =: valstream "pos" <$> reaching_state

pair_from_sim name dflt simroute = create_replicator name dflt $ valstream name <$> simroute

get_reaching_state nld tau maxtime dpos dyaw droll acc_time acc_dist route = let
  maplist = runHintedLib 10 (innerspec nld tau maxtime dpos dyaw droll acc_time acc_dist route) (repeat Map.empty)
  in fromJust.find (valstream "stop_simulation") $ maplist
