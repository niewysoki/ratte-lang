module Typechecker.Exceptions (TypecheckingException(..)) where
import Generated.Syntax
import Typechecker.Types

type Pos = BNFC'Position

data TypecheckingException
  = TypeMismatchException Pos IType IType

instance Show TypecheckingException where
  show = undefined