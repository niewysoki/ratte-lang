module Typechecker.Monads
  ( CheckerM
  , CheckerWithValueM
  , EvalM
  , EvalWithoutValueM
  , Checker(..)
  , Eval(..)
  ) where
import           Control.Monad.Except   (Except)
import           Control.Monad.Reader   (ReaderT)
import           Control.Monad.State    (StateT)
import           Typechecker.Exceptions (TypeCheckingException)
import           Typechecker.Memory     (Memory)
import           Typechecker.Types      (InternalType, ValueType)

type CheckerM' a = StateT Memory (Except TypeCheckingException) a
type CheckerM = CheckerM' ()
type CheckerWithValueM = CheckerM' ValueType

type EvalM' a = ReaderT Memory (Except TypeCheckingException) a
type EvalM = EvalM' ValueType
type EvalWithoutValueM = EvalM' ()

class Checker a where
  checkM :: Maybe InternalType -> a -> CheckerM

class Eval a where
  evalM :: a -> EvalM

