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

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["--interpret" , fname] = interpret fname
parseArgs ("--interpret" : _) = error "Interpret what?"
parseArgs ls = error "Use with --interpret FILE"
