module Typechecker.Types where

data Mutability = Imm | Mut deriving Eq

data IType
  = ITVoid
  | ITInt
  | ITStr
  | ITBool
  | ITFun [IType] IType
  deriving Eq

