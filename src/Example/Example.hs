{-# LANGUAGE RebindableSyntax  #-}
module Example.Example where

-- Lola imported as a library
import           Lola
import           Syntax.HLPrelude
import           Syntax.Booleans
import           Syntax.Ord
import           Syntax.Num
import           Lib.LTL
import           Lib.Pinescript

----------------------------------------
--
-- Hidden Definitions: We call hidden definitions to streams that cannot be
-- /observed/.
--
-- Hidden definitions are useful since we get to write streams without
-- specifying their types in the @Stream@ type, something like /intermediate steps/.
-- They can be of a non-showable type.
----------------------------------------
-- Example 1: streams in the original LOLA paper
paperEx :: [Stream Bool]
paperEx = [s11]
 where
  -- Input variables
  t1 = Input "t1"
  -- t2 = Input "t2"
  t3 = Input "t3" :: Stream Int
  -- Hidden Definitions.
  s1 = "s1" =:= True
  s2 = "s2" =: Now t3
  s3 = "s3" =: Now t1 || (Now t3 <= 1) || Leaf True
  s4 = "s4" =: (Now t3^2+7) `mod` 15
  s5 = "s5" =: if Now s3 then Now s4 else Now s4 + 1
  s6 = "s6" =: if Now t1 then Now t3 <= Now s4 else not (Now s3)
  s8 = "s8" =: t1 :@ (-1, Leaf True)
  {-s7 = "s7" =: t1 :@ (1, False)
  s9 = "s9" =: s9:@(1,0)+(Now t3 `mod` 2)
  s10 = "s10" =: Now t2 || (Now t1 && (s10:@(1,True)))-}
  s11 = "s11" =: (s11:@(1,Leaf True))
  -- s12 = "s12" =: Now t1 + Now t2 -- T1 and T2 are boolean inputs, right?
  -- Outputs variables

noinputs :: [Stream Int]
noinputs = [s1]
  where
    s1 = "s1" =: s1 :@ (-1 , 0) + 1

examplegraph :: [Stream Int]
examplegraph = [sout]
  where
  t1 = Input "t1" :: Stream Int
  s1 = "s1" =: t1 :@ (-1,0) - Now t1 + s1:@(-1,1)
  s2 = "s2" =: s3 :@ (1,0)
  s3 = "s3" =: s1 :@ (1,0)
  sout = "sout" =: Now s1 + Now s2

-- Based on GLola's amazonspec
amazonspec :: [Stream Double]
amazonspec = [percentViolations thebool]
  where
  uid = Input "uid" :: Stream String -- not used
  pid = Input "pid" :: Stream String
  stars = Input "stars" :: Stream Int
  thebool = Input "thebool" :: Stream Bool
  -- This (original) definition is wrong.
  -- We should think of a way to detect it..
  -- stars_by_prod p = "stars_by_prod" =: if App (Leaf (==p)) (Now pid) then Now stars else stars_by_prod p :@(-1,0)
  stars_by_prod p = "stars_by_prod" <: p =: if App (Leaf (==p)) (Now pid) then Now stars else stars_by_prod p :@(-1,0)
  trigger = "trigger" =:= False
  once p = "once" <: p =: Now p || once p:@(-1,Leaf False)
  alwbool = "alwbool" =: Now (historically thebool)

dynpar :: [Stream Int]
dynpar = [ou]
  where
  pid = Input "pid" :: Stream String
  pid_out = Input "pid_out" :: Stream Bool
  stars = Input "stars" :: Stream Int
  stars_by_prod p = "stars_by_prod" <: p =: if App (Leaf (==p)) (Now pid) then Now stars else stars_by_prod p :@(-1,0)
  ou = "ou" =: if Now pid_out then Now (stars_by_prod ['a']) else Now (stars_by_prod ['b'])
