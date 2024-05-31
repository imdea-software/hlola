{-# LANGUAGE RebindableSyntax  #-}
module Main where
import InFromFile
import System.IO
import Lola
import System.Environment
import Prelude
import qualified Prelude as P
import DecDyn
import Interpreter.TypedInterpreter (interpret)
import qualified Sandwich (spec)

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["--interpret" , fname] = interpret fname
parseArgs ("--interpret" : _) = error "Interpret what?"
parseArgs ls = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  let (spec, pastlist) = specfromargs ls
  runSpecJSONWithPast pastlist False spec

specfromargs args = (specfromargsold args, [])

specfromargsold ("sandwich":_) = Sandwich.spec
specfromargsold _ = error "Unkown spec"
