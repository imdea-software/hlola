module Main where
import Example.Empirical
import Engine.Engine
import System.Environment
import qualified Data.Map.Strict as Map
import DecDyn
import Data.Dynamic
import System.IO
import InFromFile
import qualified QuickCheck as QC (main)

data RunMode = Empirical (TestType, Int, Int) | QuickCheck | ExecImportedSpec | AnalyseImportedSpec | ShowHelp

importedSpec :: Specification
importedSpec = [out$smoothPeriodHeight 3]

main :: IO ()
main = parseArgs <$> getArgs >>= runInMode

parseArgs :: [String] -> RunMode
parseArgs [tt,n,m] = Empirical (read tt, read n,read m)
parseArgs ["QuickCheck"] = QuickCheck
parseArgs ["--analyse"] = AnalyseImportedSpec
parseArgs ["--execute"] = ExecImportedSpec
parseArgs _ = ShowHelp

runInMode :: RunMode -> IO ()
runInMode (Empirical (tt,n,m)) = putStrLn $ run CSV False (getSpec (tt,n)) getIn
  where
  getIn = let
    damap = Map.fromList [("p", toDyn False)]
    in
    map (const damap) [0..m]
runInMode QuickCheck = QC.main
runInMode ExecImportedSpec = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  runSpecJSON False importedSpec
runInMode AnalyseImportedSpec = analyse importedSpec
runInMode ShowHelp = putStr$unlines [
    "Wrong arguments. Usage:"
  , "  HLola --analyse"
  , "  HLola --execute"
  , "  HLola QuickCheck"
  , "  HLola TestType backref tracelen"
  , "Modify Main.hs to specify the imported the spec to --analyse or --execute."
  , "TestType must be one of (PeriodWidth | PeriodHeight | SmoothPeriodWidth | SmoothPeriodHeight | WindowTrueWidth | WindowTrueHeight), while backref and tracelen must be positive natural numbers."]
