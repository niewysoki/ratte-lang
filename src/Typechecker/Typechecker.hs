{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Typechecker where
import           Control.Monad.Except
import           Control.Monad.State
import           Generated.Syntax
import           Typechecker.Exceptions
import           Typechecker.Monads
import           Typechecker.Memory
import           Typechecker.Types
import qualified Typechecker.Checker as C
import qualified Typechecker.Evaluator as E
import Control.Monad.Reader (ReaderT(runReaderT))
import Typechecker.Common

typecheck :: Program -> Either TypeCheckingException Program
typecheck p = do
  runExcept $ evalStateT (checkM Nothing p) emptyMemory
  return p

instance Checker Program where
  checkM _ (PProgram pos inits) = do
    return ()

instance Checker Init where
  checkM _ (IFn pos id args ret block) = do
    return () 

  checkM _ (IVar pos id t exp) = do
    return ()

  checkM _ (IVarMut pos id t exp) = do
    return ()

instance Checker Block where
  checkM retT (SBlock _ stmts) = mapM_ (checkM retT) stmts

instance Checker Stmt where
  checkM _ (SEmpty _) = return ()
  
  checkM _ (SAss pos ident exp) = do
    mem <- get
    (t, mut) <- C.expectDefinedSymbolM pos ident
    (t', mut') <- eval exp mem
    assertM (t == t') (TypeMismatchE pos t t')
    assertM (mut == Mut) (ConstMismatchE pos t)
  
  checkM _ (SBStmt pos block) = do return ()

  checkM _ _ = undefined

eval :: Expr -> Memory -> CheckerWithValueM
eval exp mem = case runExcept $ runReaderT (evalM exp) mem of
  Left tce -> throwError tce
  Right t -> return t

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
