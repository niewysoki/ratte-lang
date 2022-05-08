{-# LANGUAGE LambdaCase #-}
module Main where
import           Interpreter        (interpret, interpretFile)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

main :: IO ()
main = getArgs >>= \case
  [f]        -> interpretFile f
  []         -> getContents >>= interpret
  _          -> exitFailure
