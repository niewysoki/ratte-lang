module Evaluator.Monads (EvalM, Eval(..)) where
import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (StateT)
import           Evaluator.Exceptions (RuntimeException)
import           Evaluator.Memory     (Memory, Value)

type EvalM = StateT Memory (ExceptT RuntimeException IO) Value

class Eval a where
  evalM :: a -> EvalM
