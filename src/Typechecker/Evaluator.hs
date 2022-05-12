{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Evaluator where
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Foldable
import           Data.Maybe
import           Generated.Syntax
import           Typechecker.Exceptions
import           Typechecker.Memory
import           Typechecker.Monads
import           Typechecker.Types
import           Typechecker.Common


expectMatchingArgsM :: BNFC'Position -> Ident -> [ValueType] -> [ValueType] -> EvalWithoutValueM
expectMatchingArgsM pos ident funArgTs appArgTs = do
  assertM (funArgCount == appArgCount) (ArgumentCountMismatchE pos ident funArgCount appArgCount)
  mapM_ (expectArgTypesM pos) argPairs
  mapM_ (expectArgConstM pos) argPairs
  where
    funArgCount = length funArgTs :: Int
    appArgCount = length appArgTs :: Int
    argPairs = zip funArgTs appArgTs :: [(ValueType, ValueType)]

    expectArgTypesM :: BNFC'Position -> (ValueType, ValueType) -> EvalWithoutValueM
    expectArgTypesM pos ((t, _), (t', _)) = assertM (canApply t t') $ ArgumentTypesMismatchE pos t t'

    expectArgConstM :: BNFC'Position -> (ValueType, ValueType) -> EvalWithoutValueM
    expectArgConstM pos ((t, mut), (_, mut')) = assertM (canAssign mut mut') $ ArgumentConstViolationE pos t

expectFunctionTypeM :: BNFC'Position -> InternalType -> EvalWithoutValueM
expectFunctionTypeM pos fun@(ITFun _ _) = return ()
expectFunctionTypeM pos t                    = throwError $ NotCallableE pos t

expectAndGetDefinedSymbolM :: BNFC'Position -> Ident -> EvalM
expectAndGetDefinedSymbolM pos ident = do
  maybeType <- asks (getType ident)
  case maybeType of
    Just t  -> return t
    Nothing -> throwError $ UndefinedSymbolE pos ident

expectSimpleTypesM :: BNFC'Position -> InternalType -> InternalType -> [ValueType] -> EvalM
expectSimpleTypesM pos expT retT types = do
  let t = fromMaybe expT . find (/= expT) $ map fst types
  assertM (expT == t) (InvalidTypeE pos expT t)
  return (retT, Imm)
