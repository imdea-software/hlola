{-# LANGUAGE RebindableSyntax  #-}
module Example.Bool3Ex where

-- Lola imported as a library
import           Lola
import           DecDyn
import           Syntax.HLPrelude
import           Syntax.Booleans3
import           Theories.Booleans3 (Bool3(..))
import           Syntax.Ord
import           Syntax.Num

bool3Ex :: Specification
bool3Ex = [out outstr, out p]
 where
   p = Input "p" :: Stream Bool3
   q = Input "q" :: Stream Bool3
   r = Input "r" :: Stream Bool3
   outstr = "outstr" =: (not (Now p) || q:@(-1,DK3)) && Now r
