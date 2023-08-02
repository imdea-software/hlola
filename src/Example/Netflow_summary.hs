{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Netflow_summary where
import Data.Aeson
import DecDyn
import GHC.Generics
import Lola
import Syntax.Booleans
import Syntax.HLPrelude hiding (error)
import Syntax.Num
import Syntax.Ord
import Lib.DynPar
import qualified Data.Set as Set
import qualified Prelude as P
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Engine.Engine
import qualified Example.Netflow as Nf
import Example.Attacks

spec :: Specification
spec = out fileid : out counter : concatMap get_marker ad
  where
    get_marker attack = map (out . attack_detection attack) (markers attack)

file_id :: Stream String
file_id = Input "file"

-- Output stream: file id
fileid :: Stream String
fileid = "fileid" =: Now file_id

-- Input streams: attack_ibps or attack_ipps
input_stream :: String -> Marker -> Stream Int
input_stream x y = Input (x ++ suffix (markerEnum y))


counter :: Stream Int
counter = "counter" =:
  counter :@ (-1, 0) + 1

attack_detection :: AttackData -> Marker -> Stream String
attack_detection att y = "ad_" ++ nameAtt ++ suffix (markerEnum y) =:  
  if Now (input_stream nameAtt y) > Leaf (threshold y) then
    retroactive_attack_detection (nameAtt ++ suffix (markerEnum y)) <$> Now file_id
  else
    Leaf "No attack (sum)"
  where
    nameAtt = name att
    retroactive_attack_detection attack_name file_id = runSpec (Nf.innSpec att attack_name file_id)



