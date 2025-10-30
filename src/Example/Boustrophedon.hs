{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Boustrophedon where

import Lola
import Syntax.HLPrelude
import DecDyn
import Syntax.Booleans
import Lib.Utils
import Theories.Geometry2D
import Data.Maybe
import GHC.Generics
import Data.Aeson
import qualified Prelude as P

data Position = Position {x :: Double, y :: Double, alt :: Double, zone :: Double} deriving (Show,Generic,Read,FromJSON,ToJSON)
data Wind = Wind {speed_z :: Double, speed :: Double, direction :: Double} deriving (Show,Generic,Read,FromJSON,ToJSON)

data FlyingState = Idle 
  | GoingToFirstPoint
  | GoingToSecondPoint
  deriving (Show,Eq)

data UAVEvent = StartLine | WaypointReached | NoEvent deriving (Show,Eq)

spec :: Specification
spec = [out workingpoly, out windline, out outev, out location]

evs_within :: Stream [String]
evs_within = Input "events_within"

poly :: Stream Polygon
poly = Input "poly"

position :: Stream Position
position = Input "position"
location :: Stream Point2
location = "location" =: pos2point <$> Now position
  where pos2point (Position x y _ _) = P x y

wind :: Stream Wind
wind = Input "wind"
windline :: Stream Line
windline = "windline" =: toline.direction <$> Now wind
  where
  toline a
    | a P.> pi P./ 2 = toline (a-pi)
    | a P.<= -pi P./ 2 = toline (a+pi)
    | a == pi P./ 2 = Vert 0 -- Very unlikely but, who cares?
    | otherwise = Sloped (tan a) 0


outev :: Stream (Maybe (Point2, Double))
outev = "outev" =:
  if startline then goto fst <$> Now edgesop
  else if followingline then goto snd <$> Now edgesop
  else Leaf Nothing
  where
  goto _ Nothing = Nothing
  goto f (Just (ps,_)) = Just (f ps, getangle ps)

-- goto :: Stream (Maybe (Point2, Point2))
-- goto = "goto" =: fmap fst <$> Now edgesop

edgesop :: Stream (Maybe ((Point2, Point2), Point2))
edgesop = "edgesop" =: if startline  then
  sweepPerp <$> Now location <*> Now workingpoly <*> Now windline
  else edgesop :@ (-1, Leaf Nothing)

workingpoly :: Stream Polygon
workingpoly = "workingpoly" =: let
  prevpoly = workingpoly :@ (-1,Leaf [])
  slicedpoly = slicer <$> prevpoly in
  if Now poly /== Leaf [] then Now poly
  else if followingline then maybe [] <$> slicedpoly <*> edgesop :@ (-1,Leaf undefined)
  else prevpoly

startline :: Expr Bool
startline = newstate === Leaf GoingToFirstPoint

followingline :: Expr Bool
followingline = newstate === Leaf GoingToSecondPoint

newstate :: Expr FlyingState
newstate = let
  oldst = flyingstate :@ (-1, Leaf Idle)
  newst = Now flyingstate
  in if oldst /== newst then newst else Leaf Idle

flyingstate :: Stream FlyingState
flyingstate = "flyingstate" =: let
  ev = extractEvent <$> Now evs_within in
  getNextState <$> flyingstate :@ (-1,Leaf Idle) <*> ev
  where
  extractEvent ls
    | elem "start_line" ls = StartLine -- The order is important
    | elem "wp_reached" ls = WaypointReached
    | otherwise = NoEvent
  getNextState x NoEvent = x
  getNextState Idle StartLine = GoingToFirstPoint
  getNextState GoingToFirstPoint WaypointReached = GoingToSecondPoint
  getNextState GoingToSecondPoint WaypointReached = Idle
  getNextState GoingToSecondPoint StartLine = GoingToFirstPoint
  getNextState x y = error ("Invalid state update: at " ++ show x ++ " received " ++ show y)
