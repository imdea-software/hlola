{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module INNERSPECSDIR.INNERSPEC_simuguidance where
import Lola
import GHC.Generics
import Data.Aeson
import Syntax.HLPrelude
import DecDyn (InnerSpecification(IS), bind)
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P
import Theories.GuidanceTheory
import Theories.Geometry2D
import Lib.Lola
import INNERSPECSDIR.INNERSPEC_simuinner
import Data.List.Extra


-- Custom Haskell
type Route = (Point2, Double, Maybe Point2)


simuguidance :: NonLinearData -> Double -> Double -> [Route] -> InnerSpecification ([Point2], Double, Bool, Double, Double)
simuguidance nld tau maxtime route__arg = IS [bind route route__arg] ret has_reached 5
  where

  route :: Stream Route
  route = Input "route"

  reaching_state :: Stream ReachStateType
  reaching_state = "reaching_state" =: (let
    roll = reach_roll :@(-1,(toolLift (initial_roll nld)) )
    yaw = reach_yaw :@(-1,(toolLift (initial_yaw nld)) )
    distance = reach_distance :@(-1,0)
    time = reach_time :@(-1,0)
    pos = reach_pos :@(-1,(toolLift (initial_pos nld)) )
    in
    (toolLift runSpec)  ((magic6 (simuinner nld tau maxtime))  pos yaw roll time distance (route:@(0, Leaf undefined))))

  reached_maxtime :: Stream Bool
  reached_maxtime = "reached_maxtime" =: ((toolLift reached_maxtime_field) (reaching_state:@(0, Leaf undefined)))

  reach_time :: Stream Double
  reach_time = "reach_time" =: ((toolLift reach_time_field) (reaching_state:@(0, Leaf undefined)))

  reach_distance :: Stream Double
  reach_distance = "reach_distance" =: ((toolLift reach_distance_field) (reaching_state:@(0, Leaf undefined)))

  reach_yaw :: Stream Double
  reach_yaw = "reach_yaw" =: ((toolLift reach_yaw_field) (reaching_state:@(0, Leaf undefined)))

  reach_roll :: Stream Double
  reach_roll = "reach_roll" =: ((toolLift reach_roll_field) (reaching_state:@(0, Leaf undefined)))

  reach_pos :: Stream Point2
  reach_pos = "reach_pos" =: ((toolLift reach_pos_field) (reaching_state:@(0, Leaf undefined)))

  wps :: Stream [Point2]
  wps = "wps" =: (let init_wps = [] in (toolLift snoc) (wps :@(-1, (toolLift init_wps))) (reach_pos:@(0, Leaf undefined)))

  has_reached :: Stream Bool
  has_reached = "has_reached" =: (reached_maxtime :@(-1, (toolLift False)) )

  ret :: Stream ([Point2], Double, Bool, Double, Double)
  ret = "ret" =: ((magic5 (,,,,))  (wps:@(0, Leaf undefined)) (reach_roll:@(0, Leaf undefined)) (reached_maxtime:@(0, Leaf undefined)) (reach_time:@(0, Leaf undefined)) (reach_distance:@(0, Leaf undefined)))
