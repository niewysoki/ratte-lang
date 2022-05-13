module Typechecker.Monads
  ( CheckerM
  , EmptyCheckerM
  , Checker(checkM)
  , Eval(evalM)
  ) where
import           Control.Monad.Except   (Except)
import           Control.Monad.State    (StateT)
import           Typechecker.Exceptions
import           Typechecker.Memory
import           Typechecker.Types

type CheckerM a = StateT Memory (Except TypeCheckingException) a
type EmptyCheckerM = CheckerM ()

class Checker a where
  checkM :: Maybe InternalType -> a -> CheckerM a

class Eval a where
  evalM :: a -> CheckerM ValueType
