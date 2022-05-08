module Typechecker.Checker where
import Typechecker.Monads
import Generated.Syntax
import Typechecker.Exceptions
import Typechecker.Memory
import Control.Monad.State
import Control.Monad.Except

expectDefinedSymbolM :: BNFC'Position -> Ident -> CheckerWithValueM
expectDefinedSymbolM pos ident = do
  maybeType <- gets (getType ident)
  case maybeType of
    Just t  -> return t
    Nothing -> throwError $ UndefinedSymbolE pos ident
