{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Typechecker.Typechecker (typecheck) where
import           Control.Monad.Except   (MonadError (throwError), runExcept)
import           Control.Monad.Reader   (MonadReader (ask, local),
                                         ReaderT (runReaderT))
import           Control.Monad.State    (MonadState (get, put),
                                         StateT (runStateT), evalStateT, modify)
import           Generated.Syntax
import qualified Typechecker.Checker    as Chck
import           Typechecker.Common     as Comm
import qualified Typechecker.Evaluator  as Eval
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
    mapM_ Chck.expectUniqueInitM inits
    inits' <- mapM (checkM Nothing) inits
    checkM Nothing $ SExp pos $ EApp pos (Ident "Main") []
    return $ PProgram pos inits'

instance Checker Init where
  checkM _ (IFn pos id args ret block) = do
    Comm.expectUniqueArgumentsM args $ ArgumentRedefinitionE pos
    let funT@(ITFun argTs retT) = convertType (args, ret)
    let argIds = map getArgIdent args
    modify $ addType id (funT, Imm)
    block' <- checkFunctionBodyM pos retT (zip argIds argTs) block
    return $ IFn pos id args ret block'

  checkM _ v@(IVar pos id t exp) = do
    (expT, _) <- evalM exp
    Chck.expectVarInitM pos id t expT Imm
    return v

  checkM _ v@(IVarMut pos id t exp) = do
    (expT, _) <- evalM exp
    Chck.expectVarInitM pos id t expT Mut
    return v

instance Checker Block where
  checkM retT (SBlock pos stmts) = do
    stmts' <- mapM (checkM retT) stmts
    return $ SBlock pos stmts'

instance Checker Stmt where
  checkM _ s@(SEmpty _) = return s

  checkM retT (SBStmt pos block) = do
    block' <- Chck.doNestedChecking $ checkM retT block
    return $ SBStmt pos block'

  checkM retT (SInit pos init) = do
    init' <- checkM retT init
    return $ SInit pos init'

  checkM _ s@(SDecr pos ident) = do
    Chck.expectAssignmentM pos ident ITInt
    return s

  checkM _ s@(SIncr pos ident) = do
    Chck.expectAssignmentM pos ident ITInt
    return s

  checkM Nothing (SRet pos _) = throwError $ ReturnOutOfScopeE pos
  checkM Nothing (SVRet pos) = throwError $ ReturnOutOfScopeE pos

  checkM retT (SCond pos cond block) = do
    (t, _) <- evalM cond
    Comm.assertM (t == ITBool) $ TypeMismatchE pos t ITBool
    block' <- Chck.doNestedChecking $ checkM retT block
    return $ SCond pos cond block'

  checkM retT (SCondElse pos cond blockT blockF) = do
    (t, _) <- evalM cond
    Comm.assertM (t == ITBool) $ TypeMismatchE pos t ITBool
    blockT' <- Chck.doNestedChecking $ checkM retT blockT
    blockF' <- Chck.doNestedChecking $ checkM retT blockF
    return $ SCondElse pos cond blockT' blockF'

  checkM retT (SWhile pos cond block) = do
    (t, _) <- evalM cond
    Comm.assertM (t == ITBool) $ TypeMismatchE pos t ITBool
    block' <- Chck.doNestedChecking $ checkM retT block
    return $ SWhile pos cond block'

  checkM _ s@(SExp pos exp) = do
    (t, _) <- evalM exp
    Comm.assertM (t == ITVoid) $ TypeMismatchE pos t ITVoid
    return s

  checkM _ s@(SAss pos ident exp) = do
    (t, _) <- evalM exp
    Chck.expectAssignmentM pos ident t
    return s

  checkM (Just retT) s@(SRet pos exp) = do
    (expT, _) <- evalM  exp
    Comm.assertM (canAssign retT expT) (ReturnTypeMismatchE pos retT expT)
    modify setReturn
    return s

  checkM (Just retT) s@(SVRet pos) = do
    Comm.assertM (canAssign retT ITVoid) (ReturnTypeMismatchE pos ITVoid retT)
    modify setReturn
    return s

instance Eval Expr where
  evalM (ELitInt _ _) = return (ITInt, Imm)
  evalM (ELitFalse _) = return (ITBool, Imm)
  evalM (ELitTrue _) = return (ITBool, Imm)
  evalM (EString _ _) = return (ITStr, Imm)
  evalM (ENeg pos exp) = mapM evalM [exp] >>= Eval.expectSimpleTypesM pos ITInt ITInt
  evalM (ENot pos exp) = mapM evalM [exp] >>= Eval.expectSimpleTypesM pos ITBool ITBool
  evalM (EMul pos exp1 _ exp2) = mapM evalM [exp1, exp2] >>= Eval.expectSimpleTypesM pos ITInt ITInt
  evalM (EAdd pos exp1 _ exp2) = mapM evalM [exp1, exp2] >>= Eval.expectSimpleTypesM pos ITInt ITInt
  evalM (ERel pos exp1 _ exp2) = mapM evalM [exp1, exp2] >>= Eval.expectSimpleTypesM pos ITInt ITBool
  evalM (EAnd pos exp1 exp2) = mapM evalM [exp1, exp2] >>= Eval.expectSimpleTypesM pos ITBool ITBool
  evalM (EOr pos exp1 exp2) = mapM evalM [exp1, exp2] >>= Eval.expectSimpleTypesM pos ITBool ITBool
  evalM (EVar pos ident) = Eval.expectAndGetDefinedSymbolM pos ident

  evalM (EApp pos ident args) = do
    (t, _) <- Eval.expectAndGetDefinedSymbolM pos ident
    Eval.expectFunctionTypeM pos t
    let (ITFun funArgTs retT) = t
    appArgTs <- mapM evalM args
    Eval.expectMatchingArgsM pos ident funArgTs appArgTs
    return (retT, Imm)

  evalM (ELambda pos args ret block) = do
    Comm.expectUniqueArgumentsM args $ ArgumentRedefinitionE pos
    let funT@(ITFun argTs retT) = convertType (args, ret)
    let argIds = map getArgIdent args
    checkFunctionBodyM pos retT (zip argIds argTs) block
    return (funT, Imm)

checkFunctionBodyM :: Checker a => BNFC'Position -> InternalType -> [(Ident, ValueType)] -> a -> CheckerM a
checkFunctionBodyM pos retT args block = Chck.doNestedChecking $ do
  modify $ addTypes args
  block' <- checkM (Just retT) block
  Chck.expectReturnOccuredM pos
  return block'
