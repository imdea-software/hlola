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
import Data.List.Extra
import Data.Dynamic
import Lola
import Lib.InnerSpec

type Route = (Point2, Double, Maybe Point2)

simuspec :: NonLinearData -> Double -> Double -> [Route] -> InnerSpecification ([Point2], Double, Bool, Double, Double)
simuspec nld tau maxtime routestrm = 
  IS [("route", map toDyn routestrm)] ret has_reached 5
  where
  route = Input "route" :: Declaration Route
  reaching_state = runSpec <$> (innerspec nld tau maxtime <$> pos <*> yaw <*> roll <*> time <*> distance <*> Now route)
  ----
  reached_maxtime = "reached_maxtime" =: reached_maxtime_field <$> reaching_state
  time = reach_time :@ (-1, 0)
  reach_time = "reach_time" =: reach_time_field <$> reaching_state
  distance = reach_distance :@ (-1, 0)
  reach_distance = "reach_distance" =: reach_distance_field <$> reaching_state
  yaw = reach_yaw :@ (-1, Leaf$initial_yaw nld)
  reach_yaw = "reach_yaw" =: reach_yaw_field <$> reaching_state
  roll = reach_roll :@ (-1, Leaf$initial_roll nld)
  reach_roll = "reach_roll" =: reach_roll_field <$> reaching_state
  pos = reach_pos :@ (-1, Leaf$initial_pos nld)
  reach_pos = "reach_pos" =: reach_pos_field <$> reaching_state
  wps = "wps" =: snoc <$> (wps :@ (-1, Leaf [])) <*> (Now reach_pos)
  --- Ret and stop
  has_reached = "has_reached" =: reached_maxtime :@ (-1, Leaf False)
  ret = "ret" =: (,,,,) <$> Now wps <*> Now reach_roll <*> Now reached_maxtime <*> Now reach_time <*> Now reach_distance

pair_from_sim name dflt simroute = create_replicator name dflt $ valstream name <$> simroute
