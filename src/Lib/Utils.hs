{-# LANGUAGE RebindableSyntax  #-}
module Lib.Utils where

import Lola
import qualified Prelude as P
import Syntax.Num
import Syntax.Booleans
import Syntax.HLPrelude
import Data.Dynamic
import Data.List.Extra
import Syntax.Ord

-- foldl :: (b -> a -> b) -> b -> t a -> b
hFoldl :: (Streamable a, Streamable b) => Ident -> (b->a->b) -> b -> Stream a -> Stream b
hFoldl name combiner neutral dec = name <: dec =: combiner <$> hFoldl name combiner neutral dec:@(-1,Leaf neutral) <*> Now dec

instantN :: Stream Int
instantN = "N" =: (+1) <$> instantN:@(-1,0)

plusone :: Stream Int -> Stream Int
plusone s = "plusone" <: s =: (+1) <$> Now s

instantPlusN :: Int -> Stream Int
instantPlusN n = "N+" <: n =: (+n) <$> Now instantN

pluser :: Stream Int -> Int -> Stream Int
pluser s n = "pluser" <: s <: n =: (+n) <$> Now s

instantiator5 :: (Int -> Stream Int) -> Stream Int
instantiator5 s = "instant" <: s 5 =: Now(s 5)

constStr :: (Streamable a) => Ident -> a -> Stream a
constStr name val = name =: Leaf val

strMap :: (Streamable a, Streamable b) => Ident -> (a->b) -> Stream a -> Stream b
strMap name f dec = name <: getId dec =: f <$> Now dec

slidingwin :: Typeable a => (Int, Int) -> Stream a -> Expr [a]
slidingwin (a,b) str
  | a P.< 0 P.&& b P.<= 0 = theaux (-(a+b)) :@ (b, Leaf [])
  | a P.< 0 P.&& b P.> 0 = (++) <$> theaux (-a+1) :@ (-1, Leaf []) <*> str :@@ (Leaf (b+1))
  | a P.>= 0 P.&& b P.> 0 = drop a <$> str :@@ (Leaf b)
  where
  theaux size = "slidingwin_aux" <: str <: size =: let
    myprev = theaux size :@ (-1,Leaf [])
    in snoc <$> (if Now instantN < Leaf size then myprev else tail <$> myprev) <*> Now str
