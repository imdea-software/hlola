{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.UAVSpec where
import Data.Aeson
import Data.Either
import DecDyn
import GHC.Generics
import Lola
import Syntax.Booleans
import Syntax.HLPrelude hiding (error)
import Syntax.Num
import Syntax.Ord
import Theories.Geometry2D hiding (dist)
import Theories.GuidanceTheory
import Theories.SimulatedGuidance

spec :: Specification
spec = [out intersDistance, out kfuStream, out set_speed, out simulated_guidance_wps]

navigation_data :: Stream NonLinearData
navigation_data = Input "navigation_data"

target :: Stream Point2
target = Input "target"

target_dir :: Stream Vector2
target_dir = Input "target_dir"

velocity :: Stream Vector2
velocity = Input "velocity"

position :: Stream Point2
position = Input "position"

kfuStream :: Stream (Matrix, Double, Double)
kfuStream = "kalman_filter_update" =: let
  inroll = initial_roll <$> Now navigation_data
  prev_initial_roll = initial_roll <$> navigation_data :@ (-1, Now navigation_data)
  dt = 0.05
  kfp = kalman_filter_predict <$> Now navigation_data <*> 1 <*> tau :@ (-1, 0.5)
  roll_pred = fst <$> kfp
  tau_pred = snd <$> kfp
  in
  kalman_filter_update <$> matrix_kfiltered :@ (-1, Leaf$M [[0.01,0],[0,0.01]]) <*> prev_initial_roll <*> tau :@ (-1, 0.5) <*> inroll <*> roll_pred <*> tau_pred <*> dt

matrix_kfiltered :: Stream Matrix
matrix_kfiltered = "matrix_kfiltered" =: fst3 <$> Now kfuStream
  where
  fst3 (x,_,_) = x

tau :: Stream Double
tau = "tau" =: thd3 <$> Now kfuStream
  where
  thd3 (_,_,z) = z

intersDistance :: Stream Double
intersDistance = "intersDistance" =: let
  r0 = (,) <$> Now position <*> Now velocity
  r1 = (,) <$> Now target <*> Now target_dir
  in fromRight (-1) <$> (intersectionDistance <$> r0 <*> r1)

-- Set speed

simulated_guidance :: Stream ([Point2], Double, Bool, Double, Double)
simulated_guidance = "simulated_guidance" =:
  simulate_guidance_logic <$> Now navigation_data <*> Now tau <*> 200

simulated_guidance_wps :: Stream [Point2]
simulated_guidance_wps = "simulated_guidance_wps" =:
  fst5 <$> Now simulated_guidance
  where
  fst5 (x,_,_,_,_) = x

speed_integral :: Stream (Double, Double)
speed_integral = "speed_integral" =:
  getVelIntegral <$> set_speed :@ (-1, 21) <*> integral_for_speed :@ (-1, 0) <*> Now simulated_guidance

set_speed :: Stream Double
set_speed = "set_speed" =:
  fst <$> Now speed_integral

integral_for_speed :: Stream Double
integral_for_speed = "integral_for_speed" =:
  snd <$> Now speed_integral

estim_data :: Stream Estimation
estim_data = "estim_data" =:
  simulate_guidance <$> Now tau <*> Now navigation_data

error :: Stream Double
error = "error" =:
  21 - (dist<$>Now estim_data) / (time<$>Now estim_data)

err_int :: Stream Double
err_int = "err_int" =:
  if isSaturated <$> pi_controller :@ (-1,21) <*> 15 <*> 30 then
    err_int :@ (-1,0)
  else err_int :@ (-1,0) + Now error*0.03

pi_controller :: Stream Double
pi_controller = "pi_controller" =:
  saturate <$> 21 + Now error*0.5 + Now err_int <*> 15 <*> 30
