{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Typechecker where
import           Control.Monad.Except
import           Control.Monad.Reader   (ReaderT (runReaderT))
import           Control.Monad.State
import           Generated.Syntax
import qualified Typechecker.Checker    as C
import           Typechecker.Common
import qualified Typechecker.Evaluator  as E
import           Typechecker.Exceptions
import           Typechecker.Memory
import           Typechecker.Monads
import           Typechecker.Types

typecheck :: Program -> Either TypeCheckingException Program
typecheck p = do
  runExcept $ evalStateT (checkM Nothing p) emptyMemory
  return p

instance Checker Program where
  checkM _ (PProgram pos inits) = do
    return ()

instance Checker Init where
  checkM _ (IFn pos id args ret block) = undefined
  checkM _ (IVar pos id t exp) = undefined
  checkM _ (IVarMut pos id t exp) = undefined

instance Checker Block where
  checkM retT (SBlock _ stmts) = undefined

instance Checker Stmt where
  checkM _ (SEmpty _) = return ()

  checkM _ (SBStmt pos block) = undefined
  checkM _ (SInit pos init) = undefined

  checkM _ (SAss pos ident exp) = do
    mem <- get
    (t, mut) <- C.expectDefinedSymbolM pos ident
    (t', mut') <- eval exp mem
    assertM (t == t') (TypeMismatchE pos t t')
    assertM (mut == Mut) (ConstViolationE pos t)

  checkM _ (SIncr pos ident) = do
    (t, mut) <- C.expectDefinedSymbolM pos ident
    assertM (t == ITInt) (TypeMismatchE pos ITInt t)
    assertM (mut == Mut) (ConstViolationE pos t)

  checkM t (SDecr pos ident) = checkM t (SIncr pos ident)

  checkM _ (SRet pos exp) = undefined
  checkM _ (SVRet pos) = undefined
  checkM _ (SCond pos cond block) = undefined
  checkM _ (SCondElse pos cond block block') = undefined
  checkM _ (SWhile pos cond block) = undefined
  checkM _ (SExp pos exp) = undefined

eval :: Expr -> Memory -> CheckerWithValueM
eval exp mem = case runExcept $ runReaderT (evalM exp) mem of
  Left tce -> throwError tce
  Right t  -> return t

instance Eval Expr where
  evalM (ELitInt _ _)          = return (ITInt, Imm)
  evalM (ELitFalse _)          = return (ITBool, Imm)
  evalM (ELitTrue _)           = return (ITBool, Imm)
  evalM (EString _ _)          = return (ITStr, Imm)
  evalM (ENeg pos exp)         = mapM evalM [exp] >>= E.expectTypesM pos ITInt ITInt
  evalM (ENot pos exp)         = mapM evalM [exp] >>= E.expectTypesM pos ITBool ITBool
  evalM (EMul pos exp1 _ exp2) = mapM evalM [exp1, exp2] >>= E.expectTypesM pos ITInt ITInt
  evalM (EAdd pos exp1 _ exp2) = mapM evalM [exp1, exp2] >>= E.expectTypesM pos ITInt ITInt
  evalM (ERel pos exp1 _ exp2) = mapM evalM [exp1, exp2] >>= E.expectTypesM pos ITInt ITBool
  evalM (EAnd pos exp1 exp2)   = mapM evalM [exp1, exp2] >>= E.expectTypesM pos ITBool ITBool
  evalM (EOr pos exp1 exp2)    = mapM evalM [exp1, exp2] >>= E.expectTypesM pos ITBool ITBool
  evalM (EVar pos ident)       = E.expectDefinedSymbolM pos ident

  evalM (EApp pos ident args) = do
    (t, _) <- E.expectDefinedSymbolM pos ident
    E.expectFunctionTypeM pos t
    let (ITFunction funArgTs retT) = t
    appArgTs <- mapM evalM args
    E.expectMatchingArgsM pos ident funArgTs appArgTs
    return (retT, Imm)

  evalM (ELambda pos args retT block) = undefined
