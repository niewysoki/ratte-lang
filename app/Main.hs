{-# LANGUAGE LambdaCase #-}
module Main where
import           Control.Monad      ((<=<))
import           Interpreter        (interpret)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

main :: IO ()
main = getArgs >>= \case
  [f]        -> interpret <=< readFile $ f
  []         -> getContents >>= interpret
  _          -> exitFailure
