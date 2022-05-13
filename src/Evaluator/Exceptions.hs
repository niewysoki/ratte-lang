module Evaluator.Exceptions (RuntimeException(..)) where

import           Generated.Syntax (BNFC'Position)
import Common.Utils

type Pos = BNFC'Position

data RuntimeException
  = DivideByZeroE Pos
  | UnknownE Pos

prefix :: String
prefix = "RUNTIME ERROR: "

instance Show RuntimeException where
  show (DivideByZeroE pos) = prefix ++ "division by zero at " ++ showP pos
  show (UnknownE pos)      = prefix ++ "unknown exception at " ++ showP pos
