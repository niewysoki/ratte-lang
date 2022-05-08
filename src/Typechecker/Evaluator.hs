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
  assertM (funArgCount /= appArgCount) (ArgumentCountMismatchE pos ident funArgCount appArgCount)
  mapM_ (expectArgTypesM pos) argPairs
  mapM_ (expectArgConstM pos) argPairs
  where
    funArgCount = length funArgTs :: Int
    appArgCount = length appArgTs :: Int
    argPairs = zip funArgTs appArgTs :: [(ValueType, ValueType)]

expectArgTypesM :: BNFC'Position -> (ValueType, ValueType) -> EvalWithoutValueM
expectArgTypesM pos ((t, _), (t', _)) = when (t /= t') $ throwError $ ArgumentTypesMismatchE pos t t'

expectArgConstM :: BNFC'Position -> (ValueType, ValueType) -> EvalWithoutValueM
expectArgConstM pos ((t, mut), (_, mut')) = unless (canAssign mut mut') $ throwError $ ArgumentConstMismatchE pos t

expectFunctionTypeM :: BNFC'Position -> InternalType -> EvalWithoutValueM
expectFunctionTypeM pos fun@(ITFunction _ _) = return ()
expectFunctionTypeM pos t                    = throwError $ NotCallableE pos t

expectDefinedSymbolM :: BNFC'Position -> Ident -> EvalM
expectDefinedSymbolM pos ident = do
  maybeType <- asks (getType ident)
  case maybeType of
    Just t  -> return t
    Nothing -> throwError $ UndefinedSymbolE pos ident

expectTypesM :: BNFC'Position -> InternalType -> InternalType -> [ValueType] -> EvalM
expectTypesM pos expT retT types = do
  let t = fromMaybe expT . find (/= expT) $ map fst types
  assertM (expT == t) (InvalidTypeE pos expT t)
  return (retT, Imm)
