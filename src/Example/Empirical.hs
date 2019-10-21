{-# Language RebindableSyntax #-}
module Example.Empirical where

import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Num
import DecDyn

spec :: Specification
spec = [out p, out q, out $ since 5 p q, out $ since2 5 p q]

p = Input "p"
q = Input "q"

since :: Int -> Stream Bool -> Stream Bool -> Stream Bool
since n p q
  | n==0 = name =: Now q
  | otherwise = name =: Now q || (Now p && since (n-1) p q :@ (-1,False))
  where name = "since" <: 0 <: p <: q

since2 :: Int -> Stream Bool -> Stream Bool -> Stream Bool
since2 n p q = "since2" <: n <: p <: q =: since2expr n p q
  where
  since2expr 0 _ q = Now q
  since2expr n p q = (q:@(-n, False) && (foldl (&&) (Leaf True) (map (\i -> p:@(i, True)) [(1-n)..0]))) || since2expr (n-1) p q

qsince :: Int -> Stream Bool -> Stream Bool -> Stream Int
qsince n p q
  | n==0 = name =: fromEnum <$> Now q
  | otherwise = name =: (fromEnum <$> Now q) + ((fromEnum <$> Now p) * (qsince (n-1) p q :@ (-1,0)))
  where name = "qsince" <: 0 <: p <: q

qsince2 :: Int -> Stream Bool -> Stream Bool -> Stream Int
qsince2 n p q = "qsince2" <: n <: p <: q =: qsince2expr n p q
  where
  qsince2expr 0 _ q = fromEnum <$> Now q
  qsince2expr n p q = ((fromEnum <$> q:@(-n, False)) * (foldl (*) (Leaf 1) (map (\i -> fromEnum <$> p:@(i, True)) [(1-n)..0]))) + qsince2expr (n-1) p q

data TestType = Since1 | Since2 | Since12 | QSince1 | QSince2 | QSince12 deriving Read

getSpec :: (TestType, Int) -> Specification
getSpec (tt, n) = (out p):(out q):(getStreams tt n)
  where
  getStreams Since1 n = [out $ since n p q]
  getStreams QSince1 n = [out $ qsince n p q]
  getStreams Since2 n = [out $ since2 n p q]
  getStreams QSince2 n = [out $ qsince2 n p q]
  getStreams Since12 n = [out $ since n p q, out $ since2 n p q]
  getStreams QSince12 n = [out $ qsince2 n p q, out $ qsince2 n p q]
