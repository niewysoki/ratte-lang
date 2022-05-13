{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Evaluator
  ( expectMatchingArgsM
  , expectFunctionTypeM
  , expectAndGetDefinedSymbolM
  , expectSimpleTypesM
  ) where

import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.Reader   (asks)
import           Data.Foldable          (find)
import           Data.Maybe             (fromMaybe)
import           Generated.Syntax       (BNFC'Position, Ident)
import           Typechecker.Common     (assertM)
import           Typechecker.Exceptions
import           Typechecker.Memory     (getType)
import           Typechecker.Monads     (EvalM, EvalWithoutValueM)
import           Typechecker.Types

expectMatchingArgsM :: BNFC'Position -> Ident -> [ValueType] -> [ValueType] -> EvalWithoutValueM
expectMatchingArgsM pos ident funArgTs appArgTs = do
  assertM (funArgCount == appArgCount) (ArgumentCountMismatchE pos ident funArgCount appArgCount)
  mapM_ (expectArgTypesM pos) argPairs
  where
    funArgCount = length funArgTs :: Int
    appArgCount = length appArgTs :: Int
    argPairs = zip funArgTs appArgTs :: [(ValueType, ValueType)]

expectArgTypesM :: BNFC'Position -> (ValueType, ValueType) -> EvalWithoutValueM
expectArgTypesM pos (t, t') = assertM (canAssign t t') $ ArgumentTypesMismatchE pos t t'

expectFunctionTypeM :: BNFC'Position -> InternalType -> EvalWithoutValueM
expectFunctionTypeM pos fun@(ITFun _ _) = return ()
expectFunctionTypeM pos t               = throwError $ NotCallableE pos t

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
