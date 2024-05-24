{-#LANGUAGE ScopedTypeVariables#-}
{-#LANGUAGE PolyKinds#-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE TupleSections #-}
module Interpreter.Theory where
import Interpreter.AST
import qualified Data.Map.Strict as Map
import Data.Aeson
import Control.Monad.State
import Debug.Trace
import Data.Proxy
import Data.Default
import Type.Reflection
import Text.Read (readMaybe)
import Data.Fix (Fix(..))
import qualified Type.Reflection as TR
import Control.Unification
import Control.Unification.IntVar
import Data.Constraint (Dict(..), (\\) )
import Data.Maybe
import Data.List hiding ((\\))
import Data.Char
import Data.Either.Utils

thget :: TypeDB -> Theory -> String -> ThEntry
thget typeDB syms str
  | (Just x) <- syms Map.!? str
  = x typeDB
  | otherwise = let
    ress = Map.elems (Map.map (\(DTE d) -> mayberead$toKTD d) typeDB)
    in maybe (error ("Symbol not found or readable: " ++ str)) return (listToMaybe$catMaybes ress)
    where
    mayberead (Right (KTD prx Dicts{mreaddict = Just d})) = groundreader prx str \\ d
    mayberead _ = Nothing
    groundreader (_ :: Proxy a) s = do
      ix <- readMaybe s :: Maybe a
      return (tr2pt (TR.typeOf ix), return (KnownVal ix))
-- We also could return a fresh variable and check the resulting reader, as follows:
-- thget (Theory syms rds) str = fromMaybe toberead (syms Map.!? str)
--  where
--   toberead :: ThEntry
--   toberead = do
--     a <- freshvar
--     return (a, monadicfun a)
--     where
--     monadicfun :: PolyType -> TMonadic (KnownType, Dynamic)
--     monadicfun var = do
--       tx <- applyBindings var
--       x <- orf2mon$poly2type tx
--       (KTD (_ :: Proxy a) dicts) <- orf2mon $ ktd x
--       let errtext = "Symbol \"" ++ str ++ "\" is of non readable type: " ++ show (typeRep @a)
--       readdict <- orf2mon $ mb2orf errtext (mreaddict dicts)
--       let dafun = read str \\ readdict :: a
--       let kt = KnownType (mkProxy dafun)
--       return (kt, toDyn dafun)

totheory :: Typeable a => a -> TThEntry
totheory (a::a) _ = return (tr2pt (TR.typeOf a), return (KnownVal a))

tr2pt :: TR.TypeRep a -> PolyType
tr2pt (TR.Fun x y) = let
  ptx = tr2pt x
  pty = tr2pt y
  in ptx ~> pty
tr2pt (TR.App x y) = ptx `polyapp` pty
  where
    ptx = tr2pt x
    pty = tr2pt y
tr2pt g = UTerm (TBase (show g))

-- =-=-=-=-=-= PolyFuns building tools

poly2type :: PolyType -> OrFailure Type
poly2type (UVar _) = Left "Free var in poly2type conversion"
poly2type (UTerm (TBase s)) = return $ Fix (TBase s)
poly2type (UTerm (TApp pt0 pt1)) = do
  t0 <- poly2type pt0
  t1 <- poly2type pt1
  return $ Fix (TApp t0 t1)

infixr 8 ~>
(~>) :: PolyType -> PolyType -> PolyType
x ~> y = let
  funty = ground (Proxy @(->))
  apped1 = UTerm (TApp funty x)
  apped2 = UTerm (TApp apped1 y)
  in apped2

infixl 9 `polyapp`
polyapp :: PolyType -> PolyType -> PolyType
polyapp x y = UTerm $ TApp x y

ground :: Typeable a => Proxy a -> PolyType
ground (_ :: Proxy a) = tr2pt (TR.typeRep @a)

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

freshvar :: TMonadic PolyType
freshvar = UVar <$> lift freeVar
-- -=-=-=-=-= Type DB

getKT :: TypeDB -> Type -> OrFailure KnownType
getKT typeDB ty = do
  KTD prx _ <- getKTD typeDB ty
  return (KnownType prx)

getKTD :: TypeDB -> Type -> OrFailure KTD
getKTD typeDB ty = do
  DTE a <- getDTE typeDB ty
  toKTD a

funEntry :: KTD -> KTD -> KTD
funEntry (KTD (_ :: Proxy a) _) (KTD (_ :: Proxy b) _) =
  (KTD (Proxy @(a->b)) (Dicts Nothing Nothing Nothing Nothing Nothing Nothing Nothing))

isfun :: String -> Bool
isfun = isPrefixOf "FUN "

getDTE :: TypeDB -> Type -> OrFailure DTypeEntry
getDTE db ty = getDTE' ty
  where
  getDTE' :: Type -> OrFailure DTypeEntry
  getDTE' (Fix (TBase ty))
    | isfun ty = return $ DTE funEntry
    | otherwise = do
      d <- mb2orf ("Type not found: " ++ show ty) $ db Map.!? ty
      return d
  getDTE' (Fix (TApp ty0 ty1)) = do
    DTE te0 <- getDTE' ty0
    DTE te1 <- getDTE' ty1
    base <- toKTD te1
    fun <- toFun te0
    return $ fun base

-- =-=-=-=-= Theory building

type Precedence = Double
data ParsingInfo = ParsingInfo {
  operators :: Map.Map String (String, Precedence)
  ,operatorstarts :: [Char]
}

type BuildInfo = (Theory, ParsingInfo, TypeDB)
type TheoryBuilder = State BuildInfo ()

emptytheory :: BuildInfo
emptytheory = (Map.empty, ParsingInfo Map.empty [], Map.empty)

-- =-=-=-=-=-=-=-= Theory factory functions

addSymbol :: String -> TThEntry -> TheoryBuilder
addSymbol nm entry = do
  (th,pi,tys) <- get
  let th0 = Map.insert nm entry th
  put (th0,pi,tys)

addOperator :: String -> Precedence -> TThEntry -> TheoryBuilder
addOperator nm pr entry = do
  let funame = "OP{" ++ nm ++ "}"
  addSymbol funame entry
  (et,ParsingInfo pi opstarts,tys) <- get
  let pi0 = Map.insert nm (funame, pr) pi
  let opstarts0 = head nm : opstarts
  put (et,ParsingInfo pi0 opstarts0,tys)

addTypeEntry :: String -> DTypeEntry -> TheoryBuilder
addTypeEntry nm dte = do
  (et,pi,tys) <- get
  let tys0 = Map.insert nm dte tys
  put (et,pi,tys0)

addType :: (Show a, ToJSON a, Typeable a, FromJSON a, Read a, Eq a, Ord a, Default a) => Proxy a -> TheoryBuilder
addType prx@(_ :: Proxy a) = do
  let entry = DTE (KTD prx (Dicts (Just Dict) (Just Dict) (Just Dict) (Just Dict) (Just Dict) (Just Dict) (Just Dict)))
  addTypeEntry (show (typeRep @a)) entry

typeSynonym :: (Typeable a) => String -> Proxy a -> TheoryBuilder
typeSynonym nm prx@(_ :: Proxy a) = do
  (et,pi,tys) <- get
  let Right ty = poly2type $ tr2pt (typeRep @a)
  let Right dte = getDTE tys ty
  let tys0 = Map.insert nm dte tys
  put (et,pi,tys0)

showpt :: PolyType -> String
showpt pt = fst $ showpt' [] pt
  where
  showpt' :: [(IntVar, String)] -> PolyType -> (String, [(IntVar, String)])
  showpt' fvars (UVar n) = let
    nv = fromMaybe "a" (listToMaybe (map snd fvars))
    in maybe (nv,(n,nv):fvars) ((,fvars) . snd) (find ((==n) . fst) fvars)
    where
    returnNext [c] = [chr (ord c + 1)]
  showpt' fv (UTerm (TBase s)) = (s,fv)
  showpt' fv app@(UTerm (TApp (UTerm (TApp (UTerm (TBase s)) pt0)) pt1))
    | isfun s || s == "LFunction" = let
      (s0,fv0) = showpt' fv pt0
      (s1,fv1) = showpt' fv0 pt1
      in (s0 ++ "->" ++ s1, fv1)
    | otherwise = processapp fv app
  showpt' fv app = processapp fv app
  processapp fv (UTerm (TApp pt0 pt1)) = let
    (s0,fv0) = showpt' fv pt0
    (s1,fv1) = showpt' fv0 pt1
    in (s0 ++ " " ++ s1, fv1)

printTheoryTable :: TypeDB -> Theory -> String
printTheoryTable typeDB th = let
  ls = Map.toList (Map.map (showpt . fst . fromRight . runTMonadic . ($ typeDB)) th)
  in (unlines (map show ls))
