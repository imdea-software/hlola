{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE ScopedTypeVariables#-}
module Interpreter.TypeChecker where
import Control.Unification
import Interpreter.AST
import Interpreter.Theory
import Lola (Expr, LFunction, getMayber)
import Data.Proxy
import Type.Reflection
import Data.Maybe
import Control.Arrow (left)

decodeExpr :: TypeDB -> Theory -> KnownType -> Body -> OrFailure KnownVal
decodeExpr typeDB th (KnownType (_ :: Proxy a)) b = left show $ runTMonadic $ do
  (t,e) <- inferTypes typeDB th b
  unify t $ tr2pt (typeRep @(Expr a))
  exec e

inferTypes :: TypeDB -> Theory -> Body -> TMonadic (PolyType, GBody ThFun)
inferTypes typeDB th b = inferTypes' b
  where
  inferTypes' (Symbol s) = do
    (pt,e) <- thget typeDB th s
    return (pt, Symbol e)
  inferTypes' (Application sfun sarg) = do
    (funty, efun) <- inferTypes' sfun
    (argty, earg) <- inferTypes' sarg
    b <- freshvar
    unify (argty ~> b) funty
    return (b, Application efun earg)

exec :: GBody ThFun -> TMonadic KnownVal
exec (Symbol md) = md
exec (Application bf ba) = do
  kvf <- exec bf
  kva <- exec ba
  orf2mon $ realapp kvf kva

realapp :: KnownVal -> KnownVal -> OrFailure KnownVal
realapp (KnownVal (f :: fab)) (KnownVal (x :: a))
  | App (App lfunction argt) rett <- typeRep @fab
  , Just HRefl <- eqTypeRep lfunction (typeRep @LFunction)
  , Just HRefl <- eqTypeRep argt (typeRep @a)
  = withTypeable rett $ return (KnownVal (fromJust ((getMayber f) (Just x))))
  | App (App function argt) rett <- typeRep @fab
  , Just HRefl <- eqTypeRep function (typeRep @(->))
  , Just HRefl <- eqTypeRep argt (typeRep @a)
  = withTypeable rett $ return (KnownVal (f x))
  | otherwise = Left $ "Type mismatch in realapp: " ++ show (typeRep @fab) ++ show (typeRep @a)
