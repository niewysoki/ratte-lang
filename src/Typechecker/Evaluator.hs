{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Evaluator
  ( expectMatchingArgsM
  , expectFunctionTypeM
  , expectAndGetDefinedSymbolM
  , expectSimpleTypesM
  ) where

import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.State    (gets)
import           Data.Foldable          (find)
import           Data.Maybe             (fromMaybe)
import           Generated.Syntax
import           Typechecker.Common
import           Typechecker.Exceptions
import           Typechecker.Memory
import           Typechecker.Monads
import           Typechecker.Types

expectMatchingArgsM :: BNFC'Position -> Ident -> [ValueType] -> [ValueType] -> CheckerM ()
expectMatchingArgsM pos ident funArgTs appArgTs = do
  assertM (funArgCount == appArgCount) (ArgumentCountMismatchE pos ident funArgCount appArgCount)
  mapM_ (expectArgTypesM pos) argPairs
  where
    funArgCount = length funArgTs :: Int
    appArgCount = length appArgTs :: Int
    argPairs = zip funArgTs appArgTs :: [(ValueType, ValueType)]

expectArgTypesM :: BNFC'Position -> (ValueType, ValueType) -> CheckerM ()
expectArgTypesM pos (t, t') = assertM (canAssign t t') $ ArgumentTypesMismatchE pos t t'

expectFunctionTypeM :: BNFC'Position -> InternalType -> CheckerM ()
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
