module Typechecker.Types where

data Mutability = Imm | Mut deriving Eq

data IType
  = IVoid
  | IInt Mutability
  | IStr Mutability
  | IBool Mutability
  | IFun [IType] IType Mutability
  deriving Eq

