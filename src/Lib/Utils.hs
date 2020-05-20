module Lib.Utils where

import Lola
import qualified Prelude as P
import Syntax.Num
import Syntax.HLPrelude

-- foldl :: (b -> a -> b) -> b -> t a -> b
hFoldl :: (Streamable a, Streamable b) => Ident -> (b->a->b) -> b -> Stream a -> Stream b
hFoldl name combiner neutral dec = name <: dec =: combiner <$> hFoldl name combiner neutral dec:@(-1,Leaf neutral) <*> Now dec

instantN :: Stream Int
instantN = "N" =: (+1) <$> instantN:@(-1,0)

constStr :: (Streamable a) => Ident -> a -> Stream a
constStr name val = name =: Leaf val

strMap :: (Streamable a, Streamable b) => Ident -> (a->b) -> Stream a -> Stream b
strMap name f dec = name <: getId dec =: f <$> Now dec
