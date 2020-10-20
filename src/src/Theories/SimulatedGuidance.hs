{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Theories.SimulatedGuidance where
import Engine.Engine
import Lib.InnerSpec
import Lib.SimuSpec
import Theories.GuidanceTheory
import Data.Maybe
import Data.List
import Data.Dynamic
import Theories.Geometry2D
import Data.Aeson
import GHC.Generics
import qualified Data.Map.Strict as Map

simulate_guidance_logic :: NonLinearData -> Double -> Double -> ([Point2], Double, Bool, Double, Double)
simulate_guidance_logic nld@(NLD _ wp_list _ _ num_wp radius_list _ _ _ _ _) tau maxtime = let
  pwp = Nothing : (map Just wp_list)
  wp_radius_pwp = filter farEnoughWP $ drop num_wp $ zip3 wp_list radius_list pwp
  simures = runHintedLib 10 (simuspec nld tau maxtime) [Map.singleton "route" (toDyn x) | x<- wp_radius_pwp]
  wpstates = takeUntilP1 (valstream "reached_maxtime") simures
  last_roll = last wpstates `streamval` "reach_roll"
  total_dist = last wpstates `streamval` "reach_distance"
  total_time = last wpstates `streamval` "reach_time"
  reached_max = last wpstates `streamval` "reached_maxtime"
  wps = map (valstream "reach_pos") wpstates
  in (wps, last_roll, reached_max, total_time, total_dist)
  where
  takeUntilP1 p xs = map snd $ takeWhile (not.fst) (zip (False:map p xs) xs)
  farEnoughWP (next_wp, _, Nothing) = True
  farEnoughWP (next_wp, _, Just prev_wp) = norm (next_wp `minus` prev_wp) > 0.01

kalman_filter_update :: Matrix -> Double -> Double -> Double -> Double -> Double -> Double -> (Matrix, Double, Double)
kalman_filter_update pmat prev_roll prev_tau initial_roll roll_pred tau_pred dt = let
  f = M [[prev_tau/(prev_tau + dt),dt/((prev_tau+dt)**2)],[0,1]]
  q_roll = 0.01
  q_tau = 0.00001
  r_roll = 0.001
  q = M [[q_roll,0],[0,q_tau]]
  h = M [[1,0]]
  r = M [[r_roll]]
  i = M [[1,0],[0,1]]
  x = M [[roll_pred],[tau_pred]]
  z = M [[initial_roll]]
  pmat_aux = mDot (mDot f pmat) (mTranspose f) `mPlus` q
  -- pmat_aux = F.dot(pmat).dot(F.T) + Q
  y = z `mMinus` mDot h x
  -- y = z - H.dot(x)
  s = mDot (mDot h pmat_aux) (mTranspose h) `mPlus` r
  -- S = H.dot(pmat_aux).dot(H.T) + R
  k = mDot (mDot pmat_aux (mTranspose h)) (inverse s)
  -- K = pmat_aux.dot(H.T).dot(np.linalg.inv(S))
  x_new = x `mPlus` mDot k y
  -- x_new = x + K.dot(y)
  pmat' = mDot (i `mMinus` mDot k h) pmat_aux
  -- pmat = (I - K.dot(H)).dot(pmat_aux)
  roll_filt = unm x_new!!0!!0
  tau_filt = unm x_new!!1!!0
  tau_min = 0.01
  tau_filt' = max tau_filt tau_min
  in
  (pmat',roll_filt,tau_filt')

kalman_filter_predict :: NonLinearData -> Double -> Double -> (Double, Double)
kalman_filter_predict nld@(NLD _ wp_list _ _ num_wp _ _ _ _ _ initial_roll) period_report tau
  | num_wp >= length wp_list = (initial_roll,tau)
  | otherwise = let
    maxtime = period_report*0.999
    (_,roll,reached_maxtime,_,_) = simulate_guidance_logic nld tau maxtime
    in (if reached_maxtime then roll else initial_roll, tau)

simulate_guidance :: Double -> NonLinearData -> Estimation
simulate_guidance tau nav = let
  (_, _, _, total_time, total_dist) = simulate_guidance_logic nav tau 200
  in Estimation total_time total_dist
