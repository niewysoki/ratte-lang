module Typechecker.Checker
  ( expectAssignmentM
  , expectInitTypeM
  , expectInitAnyTypeM
  , expectUniqueInitM
  , expectReturnOccuredM
  , doNestedChecking
  , expectUniqueOrShadowM
  , expectMatchingArgsM
  , expectFunctionTypeM
  , expectAndGetDefinedSymbolM
  , expectSimpleTypesM
  , expectUniqueArgumentsM
  ) where
import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.State    (MonadState (get, put), gets, modify)
import           Data.Foldable          (find)
import           Data.Maybe             (fromMaybe)
import           Generated.Syntax
import           Typechecker.Utils
import           Typechecker.Exceptions
import           Typechecker.Memory
import           Typechecker.Monads
import           Typechecker.Types

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

expectMatchingArgsM :: BNFC'Position -> Ident -> [ValueType] -> [ValueType] -> EmptyCheckerM
expectMatchingArgsM pos ident funArgTs appArgTs = do
  assertM (funArgCount == appArgCount) (ArgumentCountMismatchE pos ident funArgCount appArgCount)
  mapM_ (expectArgTypesM pos) argPairs
  where
    funArgCount = length funArgTs :: Int
    appArgCount = length appArgTs :: Int
    argPairs = zip funArgTs appArgTs :: [(ValueType, ValueType)]

expectArgTypesM :: BNFC'Position -> (ValueType, ValueType) -> EmptyCheckerM
expectArgTypesM pos (t, t') = assertM (canAssign t t') $ ArgumentTypesMismatchE pos t t'

expectFunctionTypeM :: BNFC'Position -> InternalType -> EmptyCheckerM
expectFunctionTypeM pos fun@(ITFun _ _) = return ()
expectFunctionTypeM pos t               = throwError $ NotCallableE pos t

expectAndGetDefinedSymbolM :: BNFC'Position -> Ident -> CheckerM ValueType
expectAndGetDefinedSymbolM pos ident = do
  maybeType <- gets (getType ident)
  case maybeType of
    Just t  -> return t
    Nothing -> throwError $ UndefinedSymbolE pos ident

expectSimpleTypesM :: BNFC'Position -> InternalType -> InternalType -> [ValueType] -> CheckerM ValueType
expectSimpleTypesM pos expT retT types = do
  let t = fromMaybe expT . find (/= expT) $ map fst types
  assertM (expT == t) (InvalidTypeE pos expT t)
  return (retT, Imm)

expectUniqueArgumentsM :: MonadError e m => [Arg] -> e -> m ()
expectUniqueArgumentsM args = assertM (validateFunArgs args)
