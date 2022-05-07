module Main where

import Interpreter
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [f]        -> interpretFile f
    []         -> interpretStdin
    _          -> invalidUsage


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse standard input."
    , "  (file)         Parse content of the file." ]
  exitFailure


interpretStdin :: IO ()
interpretStdin = do
  getContents >>= interpret


invalidUsage :: IO ()
invalidUsage = do
  putStrLn "Invalid usage! Use --help."
  exitFailure
