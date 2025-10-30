{-# LANGUAGE RebindableSyntax  #-}
module Example.UAVnew where

import Lola
import Syntax.HLPrelude
import DecDyn
import Lib.Utils
import Syntax.Booleans
import Theories.Geometry2D
import Theories.Discretizer
import Data.Maybe

spec :: Specification
spec = [out proposedTargets, out goTo]

reqNewTarget :: Stream Bool
reqNewTarget = Input "reqNewTarget"

goTo :: Stream Point2
goTo = Input "goTo"

offsetfield :: Stream Int
offsetfield = "offsetfield" =: if not (Now newgoto) then 0 else offsetfield :@ (-1,0) + 1

newgoto :: Stream Bool
newgoto = "newgoto" =: Now goTo /== goTo :@ (-1, Leaf (P (-1) (-1))) || Now instantN === 1 || Now goTo === Leaf (P (-1) (-1))

ufield :: Stream (Bool, Field)
ufield = let
  lastfield = field :@ (-1, Leaf defaultField)
  updf = updatefield <$> lastfield <*> Now goTo <*> Now offsetfield
  in "ufield" =: if Now newgoto then (,) <$> (isJust <$> updf) <*> (fromMaybe <$> lastfield <*> updf) else (,) <$> Leaf False <*> lastfield

field :: Stream Field
field = "field" =: snd <$> Now ufield

updated :: Stream Bool
updated = "updated" =: fst <$> Now ufield

proposedTargets :: Stream (Maybe Field)
proposedTargets = "proposedTargets" =: if Now reqNewTarget || Now updated then Just <$> Now field else Leaf Nothing
