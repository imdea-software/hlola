{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Theories.Lattice where

class Lattice x where
  sq_cup :: x -> x -> x
  sq_cap :: x -> x -> x
  mabscup :: Maybe x
  mabscap :: Maybe x

instance {-# OVERLAPPING  #-} Lattice Bool where
  sq_cup = (||)
  sq_cap = (&&)
  mabscup = Just True
  mabscap = Just False

instance {-# OVERLAPPABLE #-} (Ord a, Num a) => Lattice a where
  sq_cup = max
  sq_cap = min
  mabscup = Nothing
  mabscap = Nothing
