module Typechecker.Checker
  ( expectAssignmentM
  , expectInitTypeM
  , expectInitAnyTypeM
  , expectUniqueInitM
  , expectReturnOccuredM
  , doNestedChecking
  , expectUniqueOrShadowM
  ) where
import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.State    (MonadState (get, put), gets, modify)
import           Generated.Syntax
import           Typechecker.Common
import           Typechecker.Exceptions
import           Typechecker.Memory
import           Typechecker.Monads
import           Typechecker.Types

doNestedChecking :: CheckerM a -> CheckerM a
doNestedChecking checking = do
  mem <- get
  modify setOuterEnv
  ret <- checking
  put mem
  return ret

expectInitTypeM :: BNFC'Position -> Ident -> Type -> InternalType -> Mutability -> EmptyCheckerM
expectInitTypeM pos id t expT mut = do
  expectUniqueOrShadowM pos id
  let varT = convertType t
  assertM (expT /= ITVoid) (VoidAssignmentE pos)
  assertM (canAssign varT expT) (TypeMismatchE pos varT expT)
  modify $ addType id (expT, mut)

expectInitAnyTypeM :: BNFC'Position -> Ident -> InternalType -> Mutability -> EmptyCheckerM
expectInitAnyTypeM pos id expT mut = do
  expectUniqueOrShadowM pos id
  assertM (expT /= ITVoid) (VoidAssignmentE pos)
  modify $ addType id (expT, mut)

expectAssignmentM :: BNFC'Position -> Ident -> InternalType -> EmptyCheckerM
expectAssignmentM pos ident t = do
  (t, mut) <- expectAndGetDefinedSymbolM pos ident
  assertM (t == ITInt) (TypeMismatchE pos ITInt t)
  assertM (mut == Mut) (ConstViolationE pos t)

expectUniqueInitM :: Init -> EmptyCheckerM
expectUniqueInitM init = expectUniqueOrShadowM (initGetPos init) (initGetIdent init)

expectReturnOccuredM :: BNFC'Position -> EmptyCheckerM
expectReturnOccuredM pos = do
  ret <- gets hasReturn
  assertM ret $ NoReturnStatementE pos

expectUniqueOrShadowM :: BNFC'Position -> Ident -> EmptyCheckerM
expectUniqueOrShadowM pos ident = do
  hasSym <- gets (hasSymbolInCurrentContext ident)
  assertM (not hasSym) $ RedefinitionE pos ident

expectAndGetDefinedSymbolM :: BNFC'Position -> Ident -> CheckerM ValueType
expectAndGetDefinedSymbolM pos ident = do
  maybeType <- gets (getType ident)
  case maybeType of
    Just t  -> return t
    Nothing -> throwError $ UndefinedSymbolE pos ident
