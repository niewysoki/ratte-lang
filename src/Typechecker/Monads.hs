module Typechecker.Monads where
import Control.Monad.State (StateT)
import Control.Monad.Except (Except)
import Control.Monad.Reader (ReaderT)
import Typechecker.Persistence (Env)
import Typechecker.Exceptions (TypecheckingException)
import Typechecker.Types (IType)

type TypeMonad a b = a Env (Except TypecheckingException) b

type CheckerM = TypeMonad StateT ()
type GetterM = TypeMonad ReaderT IType
type EmptyGetterM = TypeMonad ReaderT ()

class Checker a where
  checkM :: Maybe IType -> a -> CheckerM

class Getter a where
  getM :: a -> GetterM
