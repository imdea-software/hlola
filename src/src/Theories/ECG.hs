module Theories.ECG where

type D3 = (Double, Double, Double)
type D2 = (Double, Double)

iir_df1 :: (D3, D3)
        -> (D2, D2)
        -> Double
        -> (Double, (D2, D2))
iir_df1 (b,a) (st0, st1) x = let
  (v,st0') = f3' b st0 x
  (y,st1') = iir'df1 a st1 v
  in (y,(st0',st1'))

strm2list :: (c -> a -> (b,c)) -> c -> [a] -> [b]
strm2list _ _ [] = []
strm2list combiner st (x:xs) = let
  (hd, st') = combiner st x
  in hd:strm2list combiner st' xs

iir'df1 :: D3 -> D2 -> Double -> (Double, D2)
iir'df1 a@(a0,a1,a2) (w1,w2) v = (y,w')
    where y  = v - (a1 * w1 + a2 * w2)
          w' = (y,w1)

f3' :: D3 -> D2 -> Double -> (Double, D2)
f3' h@(h0,h1,h2) (w1,w2) w0 = (y, w')
    where y  = h1 * w1 + h0 * (w0 - w2)
          w' = (w0,w1)



datita = [1..10]
testy = let
  bandpassed = strm2list (iir_df1 (( 0.16020035,  0, -0.16020035), ( 1, -1.6795993, 0.6795993))) ((0,0), (0,0)) datita
  (hd:tl) = drop 5 bandpassed
  repeatedhd = replicate 5 hd ++ tl
  diffs = differences repeatedhd
  sqdiffs = map (^2) diffs
  convolution = convolve 15 datita
  in bandpassed

differences [x] = []
differences (x:y:r) = y-x : differences (y:r)

convolve n dt = convolve' dt (replicate n 0 ++ dt) 0
  where
  convolve' [] _ _ = []
  convolve' (x:xs) (y:ys) accum = let
   newaccum = accum + x - y
   in newaccum : convolve' xs ys newaccum
