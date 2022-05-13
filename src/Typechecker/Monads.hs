module Typechecker.Monads
  ( CheckerM
  , Checker(..)
  , Eval(..)
  ) where
import           Control.Monad.Except   (Except)
import           Control.Monad.Reader   (ReaderT)
import           Control.Monad.State    (StateT)
import           Typechecker.Exceptions
import           Typechecker.Memory
import           Typechecker.Types

type CheckerM a = StateT Memory (Except TypeCheckingException) a

class Checker a where
  checkM :: Maybe InternalType -> a -> CheckerM a

class Eval a where
  evalM :: a -> CheckerM ValueType
