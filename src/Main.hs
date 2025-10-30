{-# LANGUAGE RebindableSyntax  #-}
module Main where
import InFromFile
import System.IO
import Lola
import System.Environment
import Prelude
import qualified Prelude as P
import DecDyn
-- import Example.UAVnew (spec)
import qualified Example.Netflow (spec)
import qualified Example.Netflow_summary (spec)
import qualified Example.Network (spec)
import qualified Example.Network2 (spec)
import qualified Example.Network3 (spec)
import qualified Example.Blockchain (spec)
import qualified Example.CertChain (spec)
import qualified Example.DynParEx (spec)
import qualified Example.Clustering.TzCluster (spec, mainpast)
import qualified Example.Clustering.TzClusterReason (spec, mainpast)
import qualified Example.Clustering.TzClusterForward (spec, mainpast)
import qualified Example.AskedWasMinted (spec)
import qualified Example.GeoWallet (spec)
import qualified Example.OpenEvents (spec)
import qualified Example.AcceptableUptimes (spec)
import qualified Example.AndoniEx (spec)
import qualified Example.StateWithdrawn (spec)
import qualified Example.Clustering.Test (spec, mainpast)
import qualified Example.SWSEx (spec)
import qualified Example.ListedAreOk (spec)
import qualified Example.ConsistentMetadata (spec)
import Interpreter.TypedInterpreter (interpret)
import qualified Example.Test (spec)
import qualified Example.Tezos_Execution_Order (spec)

main :: IO ()
-- main = interpret
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ls = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  let (spec, pastlist) = specfromargs ls
  runSpecJSONWithPast pastlist False spec

specfromargs ("tzcluster":_) = (Example.Clustering.TzCluster.spec, Example.Clustering.TzCluster.mainpast)
specfromargs ("tzclusterfwd":_) = (Example.Clustering.TzClusterForward.spec, Example.Clustering.TzClusterForward.mainpast)
specfromargs ("tzclusterreason":_) = (Example.Clustering.TzClusterReason.spec, Example.Clustering.TzClusterReason.mainpast)
specfromargs ("test":_) = (Example.Clustering.Test.spec, Example.Clustering.Test.mainpast)
specfromargs args = (specfromargsold args, [])

specfromargsold ("netflow":_) = Example.Netflow.spec
specfromargsold ("netflowsummary":_) = Example.Netflow_summary.spec
specfromargsold ("network":_) = Example.Network.spec
specfromargsold ("network2":_) = Example.Network2.spec
specfromargsold ("network3":_) = Example.Network3.spec
specfromargsold ("blockchain":_) = Example.Blockchain.spec
specfromargsold ("certchain":_) = Example.CertChain.spec
specfromargsold ("dynparex":_) = Example.DynParEx.spec
specfromargsold ("askedwasminted":_) = Example.AskedWasMinted.spec
specfromargsold ("geouser":addr:_) = Example.GeoWallet.spec addr
specfromargsold ("openevents":_) = Example.OpenEvents.spec
specfromargsold ("statewithdrawn":_) = Example.StateWithdrawn.spec
specfromargsold ("acceptableuptimes":_) = Example.AcceptableUptimes.spec
specfromargsold ("andoni":_) = Example.AndoniEx.spec
specfromargsold ("testdiv":_) = Example.Test.spec
specfromargsold ("tezosexecorder":_) = Example.Tezos_Execution_Order.spec
specfromargsold ("sws":_) = Example.SWSEx.spec
specfromargsold ("listedareok":_) = Example.ListedAreOk.spec
specfromargsold ("consistentMetadata":_) = Example.ConsistentMetadata.spec
specfromargsold _ = error "Unkown spec"
