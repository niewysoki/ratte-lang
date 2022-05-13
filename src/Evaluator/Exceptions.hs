module Evaluator.Exceptions (RuntimeException(..)) where

import           Generated.Syntax (BNFC'Position)

type Pos = BNFC'Position

data RuntimeException
  = DivideByZeroE Pos
  | UnkownE Pos

prefix :: String
prefix = "RUNTIME EXCEPTION: "

showP :: BNFC'Position -> String
showP (Just (x, y)) = "line " ++ show x ++ ", column " ++ show y
showP _             = "unknown position"

instance Show RuntimeException where
  show (DivideByZeroE pos) = prefix ++ "Division by zero at " ++ showP pos
  show (UnkownE pos)       = prefix ++ "Unknown exception at " ++ showP pos
