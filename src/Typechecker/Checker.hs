module Typechecker.Checker
  ( expectAndGetDefinedSymbolM
  , expectUniqueInitM
  , expectReturnOccuredM
  , expectUniqueOrShadowM
  ) where
import           Control.Monad.Except
import           Control.Monad.State
import           Generated.Syntax
import           Typechecker.Common
import           Typechecker.Exceptions
import           Typechecker.Memory
import           Typechecker.Monads
import           Typechecker.Types

expectAndGetDefinedSymbolM :: BNFC'Position -> Ident -> CheckerWithValueM
expectAndGetDefinedSymbolM pos ident = do
  maybeType <- gets (getType ident)
  case maybeType of
    Just t  -> return t
    Nothing -> throwError $ UndefinedSymbolE pos ident

expectUniqueInitM :: Init -> CheckerM
expectUniqueInitM init = expectUniqueOrShadowM (initGetPos init) (initGetIdent init)

expectReturnOccuredM :: BNFC'Position -> CheckerM
expectReturnOccuredM pos = do
  ret <- gets hasReturn
  assertM ret $ NoReturnStatementE pos

expectUniqueOrShadowM :: BNFC'Position -> Ident -> CheckerM
expectUniqueOrShadowM pos ident = do
  hasSym <- gets (hasSymbolInCurrentContext ident)
  assertM (not hasSym) $ RedefinitionE pos ident
