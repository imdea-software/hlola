{-# LANGUAGE BangPatterns #-}
module Engine.Focus (Focus (..), shiftN, emptyPast, shiftNZero) where
import Prelude
import Data.Array

-- Semi Finite implementation

-- Outside represents the access to a stream outside the boundaries.
-- Fail represents the access to a stream whose value was overwritten (and it
-- should never happen).
-- JustPos represents the access to a stream within the boundaries.
data Possible a = Outside | Fail | JustPos a deriving Show

fromMaybe :: Possible a -> Maybe a
fromMaybe Outside = Nothing
fromMaybe Fail = error "Access to forgotten system"
fromMaybe (JustPos x) = Just x

-- The Past is a circular array of fixed size and an index pointing to the next
-- value to be written.
data Past a = P {
  arr :: Array Int (Possible a),
  ind :: Int} deriving Show

maxInd :: Past a -> Int
maxInd (P arr _) = snd$bounds arr

-- Focus is a data structure that represents a stream of values where one of
-- them has the focus. The element in focus is the head of the list of future
-- values.
data Focus a = Focus { past :: Past a,
                       future :: [a],
                       intoTheFut :: Int}

-- shifting a Focus means moving the focus forward or backward, depending on the
-- sign of the argument.
shiftN :: Int -> Focus a -> Maybe (Focus a)
shiftN x | x == 0 = Just
         | x >= 0 = rshift x
         | x < 0 = lshift x

shiftNZero :: Int -> Focus a -> Maybe (Focus a)
shiftNZero x y = fmap stepon (shiftN x y)
  where
    stepon (Focus a b _) = Focus a b (max 0 (intoTheFut y))

rshift :: Int -> Focus a -> Maybe (Focus a)
rshift _ (Focus _ [_] _) = Nothing
rshift x (Focus p (f:fut) i) = let
  !pc = pastCons f p
  !newi = (i-1)
  !newx = (x-1)
  in shiftN newx (Focus pc fut  newi)

lshift :: Int -> Focus a -> Maybe (Focus a)
lshift x (Focus p@(P arr ind) f i) =
  case fromMaybe $ arr!ind of
  Nothing -> Nothing
  Just p0 -> let
    newarr = arr // [(ind, Fail)]
    newind = if ind==0 then maxInd p else ind-1
    in shiftN (x+1) (Focus (P newarr newind) (p0:f) (i+1))

pastCons :: a -> Past a -> Past a
pastCons elem p@(P arr ind) = let
  newind = mod (ind+1) (maxInd p+1)
  !newarr = arr // [(newind,JustPos elem)] in
  P newarr newind

-- Constructor method to get a Past capable of storing the amount of elements
-- specified by the argument
emptyPast :: Int -> Past a
emptyPast n = P (array (0,n) [(i, Outside) | i <- [0..n]]) 0
