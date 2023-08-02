{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Theories.Lattice where

class Lattice x where
  sqcup :: x -> x -> x
  sqcap :: x -> x -> x
  opt_top :: Maybe x
  opt_bottom :: Maybe x

instance {-# OVERLAPPING  #-} Lattice Bool where
  sqcup = (||)
  sqcap = (&&)
  opt_top = Just True
  opt_bottom = Just False

instance {-# OVERLAPPABLE #-} (Ord a, Num a) => Lattice a where
  sqcup = max
  sqcap = min
  opt_top = Nothing
  opt_bottom = Nothing
