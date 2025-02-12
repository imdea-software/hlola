{-#LANGUAGE ScopedTypeVariables#-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE TypeOperators#-}
module Interpreter.TypedInterpreter where
import InFromFile
import System.IO
import System.Environment
import Prelude
import DecDyn
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Map.Lazy as Map
import GHC.Data.Maybe
import Lola
import Debug.Trace
import Interpreter.AST
-- import Interpreter.HLolaParsing
import Interpreter.LolaParsing
import Data.Proxy
import Interpreter.Theory
import qualified Interpreter.TypeChecker as TC
import qualified Type.Reflection as TR
import Data.Constraint ((\\))
import Control.Monad.State
import Interpreter.ExampleTheory
import Interpreter.JusterTheory
import Interpreter.TzExecutionTheory
import Data.Typeable
import Data.Aeson(encode)

decodespec :: TypeDB -> Theory -> Spec -> Specification
decodespec typeDB initheory spec = catMaybes outs
  where
  ((additions, outs), theory) = (unzip $ map (addDec theory) spec, foldr ($) initheory additions)
  addDec :: Theory -> StrDec -> (Theory -> Theory, Maybe DecDyn)
  addDec theory sdec = case getExpectedType (typ sdec) (params sdec) of
    Left e -> error (show e)
    Right (KnownType prx) -> let
      dec = createFunFromDec prx theory sdec
      in (Map.insert (name sdec) (totheory dec), checkout sdec dec)
    where
    checkout (StrDec True nm ty [] _) dec = case ktd ty of
      Right (KTD (_ :: Proxy ao) Dicts{mshowdict=Just showdict, mtojsdict=Just tojsondict}) -> let
        outdec :: Stream ao = fromJust $ cast dec
        in Just $ out outdec \\ showdict \\ tojsondict
      _ -> error $ "Cannot show output stream: " ++ nm
    checkout _ _ = Nothing

  createFunFromDec :: Typeable a => Proxy a -> Theory -> StrDec -> a
  createFunFromDec _ theory z = case cffd z of
    Left err -> error err
    Right (KnownVal dec) -> fromJust $ cast dec
    where
    cffd :: StrDec -> OrFailure KnownVal
    cffd (StrDec _ nm ty [] (Just b)) = do
      kt <- getKT typeDB ty
      kvexpr <- TC.decodeExpr typeDB theory kt b
      createOutput nm kvexpr
    cffd dec@(StrDec _ nm ty ((argnm, argtynm):rargs) (Just _)) = do
        KnownType (prxret :: Proxy retty) <- getExpectedType ty rargs
        KTD (_ :: Proxy argty) dicts <- ktd argtynm
        d <- orfshowdict dicts
        let dafun = createParametricDec prxret \\ d
        return (KnownVal (dafun :: argty -> retty))
      where
      createParametricDec :: (Show x, Typeable x, Typeable y) => Proxy y -> x -> y
      createParametricDec prxret (argval :: x) = let
        newname = nm ++ "_" ++ show argval
        entry = totheory argval
        innerth = Map.insert argnm entry theory
        newdec = dec { name = newname, params = rargs }
        in createFunFromDec prxret innerth newdec
    cffd (StrDec _ nm ty [] Nothing) = do
      KTD (iprx :: Proxy sa) dicts <- ktd ty
      fjdict <- orffromjsdict dicts
      readdict <- orfreaddict dicts
      let instrm = (Input nm :: Stream sa) \\ fjdict \\ readdict
      return (KnownVal instrm)

  ktd :: Type -> OrFailure KTD
  ktd = getKTD typeDB

  getExpectedType :: Type -> [(String,Type)] -> OrFailure KnownType
  getExpectedType ty [] = do
    KnownType (_ :: Proxy a) <- getKT typeDB ty
    return (KnownType (Proxy @(Stream a)))
  getExpectedType ty ((_,x):xs) = do
    KnownType (_ :: Proxy ret) <- getExpectedType ty xs
    KnownType (_ :: Proxy a) <- getKT typeDB x
    return (KnownType (Proxy @(a -> ret)))

createOutput :: String -> KnownVal -> Either MyFailure KnownVal
createOutput nm (KnownVal (kve :: e))
  | TR.App expr x <- TR.typeRep @e
  , Just TR.HRefl <- TR.eqTypeRep expr (TR.typeRep @Lola.Expr)
  = return $ TR.withTypeable x $ KnownVal (Output (nm, kve))
  | otherwise = Left $ "Not an expression for output: " ++ show (TR.typeRep @e)

theorybuilder :: TheoryBuilder
theorybuilder =
  exampletheory
  >> justertheory
  >> tzexectheory

interpret :: String -> IO ()
interpret filename = do
  specStr <- readFile filename
  let (initheory, parsinginfo, typeDB) = execState theorybuilder emptytheory
  -- putStrLn $ printTheoryTable typeDB initheory
  let spec = strParse parsinginfo specStr
  let lolaspec = decodespec typeDB initheory spec
  -- putStrLn (unpack$encode spec)
  -- putStrLn (specStr)
  -- putStrLn "=-=-=-=-=-=-"
  runSpecJSON False lolaspec
