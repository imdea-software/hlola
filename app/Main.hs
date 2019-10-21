module Main where
-- import           Example.GeoEx
-- import           Example.Bool3Ex
-- import           Example.MITLEx
import           Example.Empirical
-- import           Example.NuSMVEx
-- import           Example.PinescriptEx
import           Engine.Engine
import System.Environment
import qualified Data.Map.Strict as Map
import DecDyn
import Data.Dynamic
-- import qualified QuickCheck as QC

main :: IO ()
-- main = QC.main
main = do
  args <- getArgs
  cont <- getContents
  (tt,n,m) <- return $ parseArgs args
  spec <- return $ getSpec (tt,n)
  putStrLn $ run CSV False spec (getIn m)
  -- putStrLn $ runSpecCSV True (getSpec $ parseArgs args) cont

parseArgs :: [String] -> (TestType, Int, Int)
parseArgs [tt,n,m] = (read tt, read n,read m)
parseArgs _ = error "Wrong number of arguments ($ HLola TestType n)"

getIn n = let
  damap = Map.fromList [("p", toDyn True), ("q", toDyn True)]
  in
  map (const damap) [0..n]
