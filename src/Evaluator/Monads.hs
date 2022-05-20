module Evaluator.Monads (EvalM, Eval(evalM), EvalEmptyM, EvalValueM) where
import           Control.Monad.Except (ExceptT)
import           Control.Monad.State  (StateT)
import           Evaluator.Exceptions (RuntimeException)
import           Evaluator.Memory     (Memory, Value)

type EvalM a = StateT Memory (ExceptT RuntimeException IO) a
type EvalEmptyM = EvalM ()
type EvalValueM = EvalM Value

class Eval a where
  evalM :: a -> EvalEmptyM
