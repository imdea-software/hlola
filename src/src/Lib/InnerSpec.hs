{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib.InnerSpec where
import Syntax.HLPrelude
import Data.Aeson
import Data.Either
import GHC.Generics
import Lola
import DecDyn
import Syntax.Ord
import Syntax.Num
import Lib.MTL
import Lib.LTL
import Syntax.Booleans
import Lib.Utils
import Theories.Geometry2D
import Theories.GuidanceTheory
import qualified Prelude as P
import qualified Data.Map.Strict as Map

data ReachStateType = RST {
  reached_maxtime_field :: Bool,
  reach_time_field :: Double,
  reach_distance_field :: Double,
  reach_yaw_field :: Double,
  reach_roll_field :: Double,
  reach_pos_field :: Point2} deriving (Generic, Show, ToJSON)

innerspec :: NonLinearData -> Double -> Double -> Point2 -> Double -> Double -> Double -> Double -> (Point2, Double, Maybe Point2) -> InnerSpecification ReachStateType
innerspec (NLD navl1_d _ navl1_p _ _ _ _ gamma vel_a vel_w _) tau maxtime (P dposx dposy) dyaw droll dtime ddistance (next_wp, wp_radius, mprev_wp) =
  IS [] ret stop_simulation 3
  where
  -- Constants
  dt = 0.05
  g = 9.8
  lim_roll_cd = 65 P.* pi P./ 180
  limit = sin lim_roll_cd
  -- Stream definitions
  reached_maxtime = "reached_maxtime" =: Now time >= Leaf maxtime && Leaf (maxtime P.> 0)
  stop_simulation = "stop_simulation" =: Now reached_maxtime || vnwp /== Leaf INTRAVEL
  time = "time" =: step_time :@ (-1,Leaf dtime)
  step_time = "step_time" =: Now time + dt
  yaw = "yaw" =: step_yaw :@ (-1,Leaf dyaw)
  step_yaw = "step_yaw" =: Now yaw + dt * yaw_dot
  step_posx = "step_posx" =: posx + dt * (Leaf vel_a * (sin <$> Now yaw) + Leaf (vel_w * sin gamma))
  step_posy = "step_posy" =: posy + dt * (Leaf vel_a * (cos <$> Now yaw) + Leaf (vel_w * cos gamma))
  pos = "pos" =: P <$> posx <*> posy
  roll = "roll" =: step_roll :@ (-1,Leaf droll)
  step_roll = "step_roll" =: (dt * demanded_roll + Leaf tau*Now roll) / (Leaf tau + dt)
  distance = "distance" =: step_distance :@ (-1,Leaf ddistance)
  step_distance = "step_distance" =: Now distance + (norm <$> (minus <$> step_pos <*> Now pos))
  ret = "ret" =: RST <$> Now reached_maxtime <*> Now time <*> Now distance <*> Now yaw <*> Now roll <*> Now pos
  -- Auxiliary expressions
  posx = step_posx :@ (-1,Leaf dposx)
  posy = step_posy :@ (-1,Leaf dposy)
  yaw_dot = (g * (sin <$> Now roll)) / Leaf vel_a
  step_pos = P <$> Now step_posx <*> Now step_posy
  vnwp = verify_next_waypoint next_wp wp_radius <$> prev_wp <*> Now pos
  vel_g = fst.get_ground_speed vel_w vel_a gamma <$> Now yaw
  hdg = snd.get_ground_speed vel_w vel_a gamma <$> Now yaw
  gcp = guidance_control_parameters navl1_d navl1_p next_wp <$> prev_wp <*> vel_g <*> hdg <*> Now pos
  l1 = fst3 <$> gcp
  k_l1 = snd3 <$> gcp
  eta = thd3 <$> gcp
  prev_wp = maybe (Now pos) Leaf mprev_wp
  demanded_roll = asin.max (-limit).min limit <$> (k_l1 * vel_g * vel_g * (sin <$> eta)) / (l1 * g)

create_replicator x y z = let
  nowstr = x =: nextstr :@ (-1, y)
  nextstr = "next_" ++ x =: z
  in (nowstr, nextstr)

fst3 (x,_,_) = x
snd3 (_,y,_) = y
thd3 (_,_,z) = z
