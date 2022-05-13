module Typechecker.Checker
  ( expectAssignmentM
  , expectVarInitM
  , expectUniqueInitM
  , expectReturnOccuredM
  , doNestedChecking
  ) where
import           Control.Monad.Except
import           Control.Monad.State
import           Generated.Syntax
import           Typechecker.Common
import           Typechecker.Exceptions
import           Typechecker.Memory
import           Typechecker.Monads
import           Typechecker.Types

doNestedChecking :: CheckerM -> CheckerM
doNestedChecking checking = do
  mem <- get
  modify setOuterEnv
  checking
  put mem

expectVarInitM :: BNFC'Position -> Ident -> Type -> InternalType -> Mutability -> CheckerM
expectVarInitM pos id t expT mut = do
  expectUniqueOrShadowM pos id
  let varT = convertType t
  assertM (canAssign varT expT) (TypeMismatchE pos varT expT)
  modify $ addType id (expT, mut)

expectAssignmentM :: BNFC'Position -> Ident -> InternalType -> CheckerM
expectAssignmentM pos ident t = do
  (t, mut) <- expectAndGetDefinedSymbolM pos ident
  assertM (t == ITInt) (TypeMismatchE pos ITInt t)
  assertM (mut == Mut) (ConstViolationE pos t)

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

expectAndGetDefinedSymbolM :: BNFC'Position -> Ident -> CheckerWithValueM
expectAndGetDefinedSymbolM pos ident = do
  maybeType <- gets (getType ident)
  case maybeType of
    Just t  -> return t
    Nothing -> throwError $ UndefinedSymbolE pos ident
