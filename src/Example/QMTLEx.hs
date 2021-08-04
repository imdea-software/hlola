{-# LANGUAGE RebindableSyntax  #-}
module Example.QMTLEx where

import Lola
import Syntax.HLPrelude
import DecDyn
import Lib.QMTL
import Syntax.Booleans

specification :: Specification
specification = [out qprevex]

prevex :: Stream Bool
prevex =  "prevex" =: Now (previously (-1,1) phi)

phi :: Stream Bool
phi = Input "phi"

psi :: Stream Bool
psi = Input "psi"

sinceex :: Stream Bool
sinceex =  "sinceex" =: Now (since (-1,1) phi psi)

qphi :: Stream Int
qphi = Input "qphi"

qprevex :: Stream Int
qprevex =  "qprevex" =: Now (previously (-1,1) qphi)
