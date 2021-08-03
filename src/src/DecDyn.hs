{-#LANGUAGE ScopedTypeVariables#-}
module DecDyn where

import Data.Dynamic
import Lola
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types

type Readers = Map.Map Ident (String -> Dynamic)
type FromJSONers = Map.Map Ident (Value -> Dynamic)
type DecDyn = (DeclarationDyn, (Dynamic -> String, Dynamic -> Value), Readers, FromJSONers)

type Specification = [DecDyn]

data Format = JSON | CSV deriving Eq

-- We overwrite all the data information to work only with Dynamic values.
-- The type checking has already taken place and if the input is of the correct
-- type, it guarantees that no type mismatch can occur in runtime.
data ExprDyn where
  DLeaf :: Dynamic -> ExprDyn
  DApp  :: (Dynamic, Dynamic, Dynamic, Dynamic -> Maybe Dynamic) -> ExprDyn -> ExprDyn -> ExprDyn
  DNow :: DeclarationDyn -> ExprDyn
  DAt :: DeclarationDyn -> (Int, ExprDyn) -> ExprDyn
  DSlice :: (Dynamic, Dynamic -> Dynamic -> Dynamic, [Dynamic] -> Dynamic) -> DeclarationDyn -> ExprDyn -> ExprDyn

data DeclarationDyn where
  DInp :: Ident -> DeclarationDyn
  DOut :: (Ident, ExprDyn) -> DeclarationDyn

instance Show ExprDyn where
  show (DLeaf a) = "Leaf"
  show (DApp _ e1 e2) = "App (" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (DNow dec) = "Now " ++ show (dgetId dec)
  show (DAt dec (i,d)) = show (dgetId dec) ++ "[" ++ show i ++ "|default]"

instance Show DeclarationDyn where
  show (DInp id) = "Input " ++ show id
  show (DOut (id, exp)) = "Output " ++ show id ++ ": " ++ show exp

out :: forall a.(Streamable a, Show a, ToJSON a) => Declaration a -> DecDyn
out = dec2Dyn

dgetId (DInp x) = x
dgetId (DOut (x,_)) = x

dec2Dyn :: forall a.(Streamable a, Show a, ToJSON a) => Declaration a -> DecDyn
dec2Dyn dec = let
  (ed,thereaders,thejsoners) = dec2Dyn' (Map.empty) dec Map.! getId dec
  thea = fromMaybe (error "020").(fromDynamic :: Dynamic -> Maybe a) in
  (ed, (show.thea, toJSON.thea), thereaders, thejsoners)

type DecMap = Map.Map Ident (DeclarationDyn, Readers, FromJSONers)
dec2Dyn' :: forall a. DecMap -> Declaration a -> DecMap
dec2Dyn' themap dec | Map.member (getId dec) themap = themap
dec2Dyn' themap dec@(Input id) = Map.insert id (DInp id, Map.singleton id (toDyn.(read :: String -> a)), Map.singleton id (toDyn.(fromMaybe (error "Wrong type in JSON").parseMaybe parseJSON :: Value -> a))) themap
dec2Dyn' themap dec@(Output (id,exp)) = let
  outdec = DOut (id, ddec)
  (themap2, ddec, readers, jsoners) = exp2Dyn exp (Map.insert id (outdec, Map.empty, Map.empty) themap) in
  Map.insert id (outdec, readers, jsoners) themap2

exp2Dyn :: Expr a -> DecMap -> (DecMap, ExprDyn, Readers, FromJSONers)
exp2Dyn (Leaf x) m = (m, DLeaf (toDyn x), Map.empty, Map.empty)
exp2Dyn (App (e1 :: Expr (f a b)) e2) m = let
  (m1, de1, r1, j1) = exp2Dyn e1 m
  (m2, de2, r2, j2) = exp2Dyn e2 m1
  sndWhenMatched = MM.zipWithMatched (const$const id)
  mymergereaders = MM.merge MM.preserveMissing MM.preserveMissing sndWhenMatched
  mymergejsoners = MM.merge MM.preserveMissing MM.preserveMissing sndWhenMatched
  unlifter = (fmap toDyn).(fromJust.fromDynamic :: Dynamic -> Maybe b)
  in
  (m2, DApp (toDyn (getMayber.toLFunction :: f a b -> Maybe a -> Maybe b), toDyn (Just :: a -> Maybe a), toDyn (Nothing :: Maybe a), unlifter) de1 de2, mymergereaders r1 r2, mymergejsoners j1 j2)
exp2Dyn (Now dec) m = let
  m1 = dec2Dyn' m dec
  (ddec, readers, jsoners) = m1 Map.! getId dec in
  (m1, DNow ddec, readers, jsoners)
exp2Dyn (dec :@ (0,_)) m = exp2Dyn (Now dec) m -- optimization
exp2Dyn (dec :@ (i,d)) m = let
  m1 = dec2Dyn' m dec
  (ddec, readers, jsoners) = m1 Map.! getId dec
  (m2, de, r1, j1) = exp2Dyn d m1
  sndWhenMatched = MM.zipWithMatched (const$const id)
  mymergereaders = MM.merge MM.preserveMissing MM.preserveMissing sndWhenMatched
  mymergejsoners = MM.merge MM.preserveMissing MM.preserveMissing sndWhenMatched
  in
  (m2,DAt (ddec) (i,de), mymergereaders readers r1, mymergejsoners jsoners j1)
exp2Dyn (dec :@@ len) m = let
  m1 = dec2Dyn' m dec
  (ddec, readers, jsoners) = m1 Map.! getId dec
  (m2, de, r1, j1) = exp2Dyn len m1
  sndWhenMatched = MM.zipWithMatched (const$const id)
  mymergereaders = MM.merge MM.preserveMissing MM.preserveMissing sndWhenMatched
  mymergejsoners = MM.merge MM.preserveMissing MM.preserveMissing sndWhenMatched
  slicetools = getSliceTools dec
  in
  (m2,DSlice slicetools ddec de, mymergereaders readers r1, mymergejsoners jsoners j1)

getSliceTools :: Typeable a => Declaration a -> (Dynamic, Dynamic -> Dynamic -> Dynamic, [Dynamic] -> Dynamic)
getSliceTools (dec :: Declaration a) = (toDyn ([] :: [a]), \x xs -> dynApp (dynApp (toDyn ((:) :: a -> [a] -> [a])) x) xs,
  \ls -> toDyn (map (fromJust.fromDynamic :: Dynamic -> a) ls))

fst4 (a,_,_,_) = a
snd4 (_,b,_,_) = b
thd4 (_,_,c,_) = c
fth4 (_,_,_,d) = d

getReaders :: Specification -> Readers
getReaders decs = let
  sndWhenMatched = MM.zipWithMatched (const$const id)
  mymerge = (MM.merge MM.preserveMissing MM.preserveMissing sndWhenMatched)
  readers = map thd4 decs in
  foldl mymerge Map.empty readers

getFromJSONers :: Specification -> FromJSONers
getFromJSONers decs = let
  sndWhenMatched = MM.zipWithMatched (const$const id)
  mymerge = (MM.merge MM.preserveMissing MM.preserveMissing sndWhenMatched)
  readers = map fth4 decs in
  foldl mymerge Map.empty readers

data InnerSpecification a where
 IS :: (Typeable a, Show a, ToJSON a) => {
    ins :: [(Ident, [Dynamic])],
    retStream :: Stream a,
    stopStream :: Stream Bool,
    hint :: Int
  } -> InnerSpecification a

getDecs :: Typeable a => InnerSpecification a -> Specification
getDecs (IS _ rs ss _) = [out rs, out ss]

getIns :: InnerSpecification a -> [Map.Map Ident Dynamic]
getIns (IS [] _ _ _) = repeat Map.empty
getIns is = let
  idwithin = [[(id,dyn) | dyn <- dyns] | (id, dyns) <- ins is] :: [[(Ident, Dynamic)]]
  in createMap idwithin

createMap :: [[(Ident, Dynamic)]] -> [Map.Map Ident Dynamic]
createMap inis
  | any null inis = []
  | otherwise = Map.fromList (map head inis) : createMap (map tail inis)

getFromDynner :: Typeable a => InnerSpecification a -> Dynamic -> a
getFromDynner _ d = fromDyn d undefined

bind :: Typeable a => Declaration a -> [a] -> (Ident, [Dynamic])
bind str vals = (getId str, map toDyn vals)

