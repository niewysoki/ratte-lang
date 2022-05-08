module Typechecker.Types where
import Data.List

type ValueType = (InternalType, Mutability)

data Mutability = Imm | Mut deriving Eq

data InternalType
  = ITEmpty
  | ITVoid
  | ITInt
  | ITStr
  | ITBool
  | ITFunction [ValueType] InternalType
  deriving Eq

instance Show Mutability where
  show Mut = "mut"
  show Imm = ""

instance Show InternalType where
  show ITVoid = "Void"
  show ITInt = "Int"
  show ITStr = "Str"
  show ITBool = "Bool"
  show (ITFunction argTs retT) = "(" ++ showArgs argTs ++ ") -> " ++ show retT where
    showArgs = intercalate ", " . map showArg
    showArg (t, mut) = show mut ++ " " ++ show t
  show _ = "Not possible to see"

