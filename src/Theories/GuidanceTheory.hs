{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Theories.GuidanceTheory where
import Data.Maybe
import Data.List
import Theories.Geometry2D
import Data.Aeson
import GHC.Generics

data NonLinearData = NLD {
  navl1_d :: Double, wp_list :: [Point2], navl1_p :: Double, initial_yaw :: Double, num_wp :: Int, radius_list :: [Double],
  initial_pos :: Point2, gamma :: Double, vel_a :: Double, vel_w :: Double, initial_roll :: Double} deriving (Show,Generic,Read,FromJSON,ToJSON)

data UAVTravelStatus = REACHED | PASSED | INTRAVEL deriving Eq

data EstimatedArrival = DontKnowIfWillArrive | WillArriveClose | WillArriveFar | NotGoingToArrive | BorderCaseArrival deriving (Show,Generic,Read,FromJSON,ToJSON)
data Estimation = Estimation {time,dist :: Double}

clip :: Double -> Double -> Double -> Double
clip x y z = max y.min z $ x
saturate :: Double -> Double -> Double -> Double
saturate = clip
isSaturated x y z = x==y || x==z

verify_next_waypoint :: Point2 -> Double -> Point2 -> Point2 -> UAVTravelStatus
verify_next_waypoint next_wp wp_radius prev_wp pos
  | norm vec_UB <= wp_radius = REACHED
  | alongtrack >= mod_AB = PASSED
  | otherwise = INTRAVEL
  where
  vec_UB = next_wp `minus` pos
  vec_AB = next_wp `minus` prev_wp
  mod_AB = norm vec_AB
  vec_AU = pos `minus` prev_wp
  alongtrack = dot vec_AU vec_AB / mod_AB

guidance_control_parameters :: Double -> Double -> Point2 -> Point2 -> Double -> Double -> Point2 -> (Double, Double, Double)
guidance_control_parameters navl1_d navl1_p next_wp prev_wp vel_g hdg pos = (l1,k_l1,eta)
  where
  l1 = (navl1_d * navl1_p * vel_g) / pi
  k_l1 = 4.0 * navl1_d * navl1_d
  vec_gs = P (vel_g * sin hdg) (vel_g * cos hdg)
  vec_UA = prev_wp `minus` pos
  vec_AB = next_wp `minus` prev_wp
  mod_UA = norm vec_UA
  ang_prev = angle_between_vectors vec_AB vec_UA
  region_I = (mod_UA >= l1 && ang_prev >= (-pi)/4 && ang_prev <= pi/4)
  eta_region_I = - angle_between_vectors vec_gs vec_UA
  crosstrack = cross (P 0 0 `minus` vec_UA) vec_AB / norm vec_AB
  sin_eta1 = crosstrack/l1
  sin_45 = sin (pi/4)
  eta1 = asin $ clip sin_eta1 (-sin_45) sin_45
  eta2 = angle_between_vectors vec_gs vec_AB
  eta = if region_I then eta_region_I else (-(eta1 + eta2))

get_ground_speed :: Double -> Double -> Double -> Double -> (Double, Double)
get_ground_speed vel_w vel_a gamma yaw = (vel_g, hdg)
  where
  vec_w = P (vel_w * sin gamma) (vel_w * cos gamma)
  vec_a = P (vel_a * sin yaw) (vel_a * cos yaw)
  vec_g@(P vecg0 vecg1) = vec_a `plus` vec_w
  vel_g = norm vec_g
  hdg = atan2 vecg0 vecg1

data Matrix = M [[Double]] deriving (Show, ToJSON, Generic)
unm (M xs) = xs

mTranspose :: Matrix -> Matrix
mTranspose (M xs) = M (transpose xs)
mDot :: Matrix -> Matrix -> Matrix
mDot (M a) (M b) = M [[sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a]
mPlus :: Matrix -> Matrix -> Matrix
mPlus (M a) (M b) = M $ zipWith (zipWith (+)) a b
mMinus :: Matrix -> Matrix -> Matrix
mMinus (M a) (M b) = M $ zipWith (zipWith (-)) a b
inverse :: Matrix -> Matrix
inverse (M [[x]])= M [[1/x]]

-- Speed control
getVelIntegral :: Double -> Double -> ([Point2], Double, Bool, Double, Double) -> (Double, Double)
getVelIntegral target_vel integral (_, _, _, 0, _) = (target_vel, integral)
getVelIntegral target_vel integral (_, _, _, total_time, total_dist) = (newtarget_vel, newintegral)
  where
  vel_a = 21
  vel_min = 15
  vel_max = 30
  prop_gain = 0.5
  int_gain = 0.03
  int_max = 2
  error = vel_a - total_dist/total_time
  newintegral = if not (target_vel == vel_min || target_vel == vel_max) then clip (integral + int_gain * error) (-int_max) int_max else integral
  newtarget_vel = clip (error*prop_gain + vel_a + newintegral) vel_min vel_max

check_arrival :: NonLinearData -> [Point2] -> EstimatedArrival
check_arrival (NLD _ wp_list _ _ num_wp radius_list _ _ _ _ _) positions
  | length positions == length wp_list - num_wp =
    if num_wp < length wp_list `div` 2 then
      DontKnowIfWillArrive
    else if distance (last positions) (last wp_list) < 1.5 * (last radius_list) then
      WillArriveClose
    else
      WillArriveFar
  | num_wp < length wp_list = NotGoingToArrive
  | otherwise = BorderCaseArrival
