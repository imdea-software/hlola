{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module INNERSPECSDIR.INNERSPEC_simuinner where
import Lola
import GHC.Generics
import Data.Aeson
import Syntax.HLPrelude
import DecDyn (InnerSpecification(), createIS, bind)
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P
import Theories.GuidanceTheory hiding (time)
import Theories.Geometry2D hiding (distance)


-- Custom datas

data ReachStateType = RST {
  reached_maxtime_field :: Bool,
  reach_time_field :: Double,
  reach_distance_field :: Double,
  reach_yaw_field :: Double,
  reach_roll_field :: Double,
  reach_pos_field :: Point2} deriving (Generic, Show, ToJSON)

-- Custom Haskell


simuinner ::  NonLinearData -> Double -> Double -> Point2 -> Double -> Double -> Double -> Double -> (Point2, Double, Maybe Point2) ->  InnerSpecification ReachStateType
simuinner nld tau maxtime dpos dyaw droll dtime ddistance wps  = createIS [] ret stop_simulation (3)
  where
  dt = 0.05
  g = 9.8
  lim_roll_cd = 65 P.* pi P./ 180
  limit = sin lim_roll_cd
  (NLD navl1_d _ navl1_p _ _ _ _ gamma vel_a vel_w _) = nld
  (P dposx dposy) = dpos
  (next_wp, wp_radius, mprev_wp) = wps


  reached_maxtime :: Stream Bool
  reached_maxtime = "reached_maxtime" =: ((time:@(0, Leaf undefined))  >= (toolLift maxtime) && (toolLift (maxtime P.> 0)) )

  stop_simulation :: Stream Bool
  stop_simulation = "stop_simulation" =: (let
    vnwp = (toolLift (verify_next_waypoint next_wp wp_radius))   (prev_wp:@(0, Leaf undefined))   (pos:@(0, Leaf undefined)) 
    in  (reached_maxtime:@(0, Leaf undefined))  || vnwp /== (toolLift INTRAVEL) )

  time :: Stream Double
  time = "time" =: ((step_time:@(-1,(toolLift dtime))) )

  step_time :: Stream Double
  step_time = "step_time" =: ((time:@(0, Leaf undefined))  + dt)

  yaw :: Stream Double
  yaw = "yaw" =: ((step_yaw:@(-1,(toolLift dyaw))) )

  step_yaw :: Stream Double
  step_yaw = "step_yaw" =: (let
    yaw_dot = (g * ((toolLift sin) ( (roll:@(0, Leaf undefined)) ))) / (toolLift vel_a)
    in  (yaw:@(0, Leaf undefined))  + dt * yaw_dot)

  step_posx :: Stream Double
  step_posx = "step_posx" =: ((posx:@(0, Leaf undefined))  + dt * ((toolLift vel_a) * ((toolLift sin)  (yaw:@(0, Leaf undefined)) ) + (toolLift (vel_w * sin gamma)) ))

  step_posy :: Stream Double
  step_posy = "step_posy" =: ((posy:@(0, Leaf undefined))  + dt * ((toolLift vel_a) * ((toolLift cos)  (yaw:@(0, Leaf undefined)) ) + (toolLift (vel_w * cos gamma)) ))

  pos :: Stream Point2
  pos = "pos" =: ((toolLift P)  (posx:@(0, Leaf undefined))   (posy:@(0, Leaf undefined)))

  roll :: Stream Double
  roll = "roll" =: ((step_roll:@(-1,(toolLift droll))) )

  step_roll :: Stream Double
  step_roll = "step_roll" =: (let
    vel_g = (toolLift (fst.get_ground_speed vel_w vel_a gamma))   (yaw:@(0, Leaf undefined)) 
    hdg = (toolLift (snd.get_ground_speed vel_w vel_a gamma))   (yaw:@(0, Leaf undefined)) 
    gcp = (magic4 (guidance_control_parameters navl1_d navl1_p next_wp))   (prev_wp:@(0, Leaf undefined))  vel_g hdg  (pos:@(0, Leaf undefined)) 
    fst3 (x,_,_) = x
    snd3 (_,y,_) = y
    thd3 (_,_,z) = z
    l1 = (toolLift fst3) gcp
    k_l1 = (toolLift snd3) gcp
    eta = (toolLift thd3) gcp
    demanded_roll = (toolLift (asin.max (-limit).min limit))  ((k_l1 * vel_g * vel_g * ((magic1 sin) eta)) / (l1 * g))
    in
    (dt * demanded_roll + (toolLift tau) *  (roll:@(0, Leaf undefined)) ) / ((toolLift tau) + dt))

  distance :: Stream Double
  distance = "distance" =: ((step_distance:@(-1,(toolLift ddistance))) )

  step_distance :: Stream Double
  step_distance = "step_distance" =: (let
    step_pos = (toolLift P)  (step_posx:@(0, Leaf undefined))   (step_posy:@(0, Leaf undefined)) 
    in  (distance:@(0, Leaf undefined))  + (toolLift norm) ((magic2 minus) step_pos  (pos:@(0, Leaf undefined)) ))

  ret :: Stream ReachStateType
  ret = "ret" =: ((magic6 RST)  (reached_maxtime:@(0, Leaf undefined))   (time:@(0, Leaf undefined))   (distance:@(0, Leaf undefined))   (yaw:@(0, Leaf undefined))   (roll:@(0, Leaf undefined))   (pos:@(0, Leaf undefined)))

  posx :: Stream Double
  posx = "posx" =: ((step_posx:@(-1, (toolLift dposx))) )

  posy :: Stream Double
  posy = "posy" =: ((step_posy:@(-1, (toolLift dposy))) )

  prev_wp :: Stream Point2
  prev_wp = "prev_wp" =: (maybe  (pos:@(0, Leaf undefined))  Leaf mprev_wp)
