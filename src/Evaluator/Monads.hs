module Evaluator.Monads (EvalM, Eval(..)) where
import           Control.Monad.Except
import           Control.Monad.State
import           Evaluator.Exceptions
import           Evaluator.Memory

type EvalM = StateT Memory (ExceptT RuntimeException IO) Value

class Eval a where
  evalM :: a -> EvalM
