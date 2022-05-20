{-# LANGUAGE FlexibleInstances     #-}
module Typechecker.Typechecker (typecheck) where
import           Control.Monad.Except   (MonadError (throwError), runExcept)
import           Control.Monad.State    (MonadState (get, put),
                                         StateT (runStateT), evalStateT, modify, gets, when)
import           Data.List
import           Generated.Syntax
import           Typechecker.Checker
import           Typechecker.Exceptions
import           Typechecker.Memory
import           Typechecker.Monads
import           Typechecker.Types
import           Typechecker.Utils

typecheck :: Program -> Either TypeCheckingException Program
typecheck p = runExcept $ evalStateT (checkM Nothing p) emptyMemory

instance Checker Program where
  checkM _ (PProgram pos inits) = do
    expectUniqueInitsM inits
    inits' <- mapM (checkM Nothing) inits
    checkM Nothing $ SExp pos $ EApp pos (Ident "Main") []
    return $ PProgram pos inits'

instance Checker Init where
  checkM _ (IFn pos id args ret block) = do
    expectUniqueArgumentsM args $ ArgumentRedeclarationE pos
    let funT@(ITFun argTs retT) = convertType (args, ret)
    let argIds = map getArgIdent args
    modify $ addType id (funT, Imm)
    block' <- checkFunctionBodyM pos retT (zip argIds argTs) block
    return $ IFn pos id args ret block'

  checkM _ v@(IConst pos id t exp) = do
    (expT, _) <- evalM exp
    expectInitTypeM pos id t expT Imm
    return $ IVarInf pos id exp

  checkM _ (IVar pos id t exp) = do
    (expT, _) <- evalM exp
    expectInitTypeM pos id t expT Mut
    return $ IVarInf pos id exp

  checkM _ v@(IConstInf pos id exp) = do
    (expT, _) <- evalM exp
    expectInitAnyTypeM pos id expT Imm
    return $ IVarInf pos id exp

  checkM _ v@(IVarInf pos id exp) = do
    (expT, _) <- evalM exp
    expectInitAnyTypeM pos id expT Mut
    return v

instance Checker Block where
  checkM retT (SBlock pos stmts) = do
    stmts' <- mapM (checkM retT) stmts
    return $ SBlock pos stmts'

instance Checker Stmt where
  checkM _ s@(SEmpty _) = return s

  checkM retT (SBStmt pos block) = do
    block' <- doNestedChecking $ checkM retT block
    return $ SBStmt pos block'

  checkM retT (SInit pos init) = do
    init' <- checkM retT init
    return $ SInit pos init'

  checkM _ s@(SDecr pos ident) = do
    expectAssignmentM pos ident ITInt
    return s

  checkM _ s@(SIncr pos ident) = do
    expectAssignmentM pos ident ITInt
    return s

  checkM retT (SCond pos cond block) = do
    (t, _) <- evalM cond
    assertM (t == ITBool) $ TypeMismatchE pos t ITBool
    block' <- doNestedChecking $ checkM retT block
    return $ SCond pos cond block'

  checkM retT (SCondElse pos cond blockT blockF) = do
    (t, _) <- evalM cond
    assertM (t == ITBool) $ TypeMismatchE pos t ITBool
    (blockT', hasReturnT) <- doNestedCheckingWithReturn $ checkM retT blockT
    (blockF', hasReturnF) <- doNestedCheckingWithReturn $ checkM retT blockF
    when (hasReturnT && hasReturnF) $ modify setReturn
    return $ SCondElse pos cond blockT' blockF'

  checkM retT (SWhile pos cond block) = do
    (t, _) <- evalM cond
    assertM (t == ITBool) $ TypeMismatchE pos t ITBool
    block' <- doNestedChecking $ checkM retT block
    return $ SWhile pos cond block'

  checkM _ s@(SExp pos exp) = do
    (t, _) <- evalM exp
    assertM (t == ITVoid) $ TypeMismatchE pos t ITVoid
    return s

  checkM _ s@(SAss pos ident exp) = do
    (t, _) <- evalM exp
    expectAssignmentM pos ident t
    return s

  checkM Nothing (SRet pos _) = throwError $ ReturnOutOfScopeE pos
  checkM Nothing (SVRet pos)  = throwError $ ReturnOutOfScopeE pos

  checkM (Just retT) s@(SRet pos exp) = do
    (expT, _) <- evalM  exp
    assertM (canAssign retT expT) (ReturnTypeMismatchE pos retT expT)
    modify setReturn
    return s

  checkM (Just retT) s@(SVRet pos) = do
    assertM (retT == ITVoid) (ReturnTypeMismatchE pos ITVoid retT)
    modify setReturn
    return s

instance Eval Expr where
  evalM (ELitInt _ _)                 = return (ITInt, Imm)
  evalM (ELitFalse _)                 = return (ITBool, Imm)
  evalM (ELitTrue _)                  = return (ITBool, Imm)
  evalM (EString _ _)                 = return (ITStr, Imm)
  evalM (ENeg pos exp)                = evalSimpleExprs pos [exp] [ITInt] ITInt
  evalM (ENot pos exp)                = evalSimpleExprs pos [exp] [ITBool] ITBool
  evalM (EAdd pos exp1 _ exp2)        = evalSimpleExprs pos [exp1, exp2] [ITInt] ITInt
  evalM (EMul pos exp1 _ exp2)        = evalSimpleExprs pos [exp1, exp2] [ITInt] ITInt
  evalM (EAnd pos exp1 exp2)          = evalSimpleExprs pos [exp1, exp2] [ITBool] ITBool
  evalM (EOr pos exp1 exp2)           = evalSimpleExprs pos [exp1, exp2] [ITBool] ITBool
  evalM (ERel pos exp1 (OEQU _) exp2) = evalSimpleExprs pos [exp1, exp2] [ITStr, ITBool, ITInt] ITBool
  evalM (ERel pos exp1 (ONE _) exp2)  = evalSimpleExprs pos [exp1, exp2] [ITStr, ITBool, ITInt] ITBool
  evalM (ERel pos exp1 _ exp2)        = evalSimpleExprs pos [exp1, exp2] [ITInt] ITBool
  evalM (EVar pos ident)              = expectAndGetDefinedSymbolM pos ident

  evalM (EApp pos ident args) = do
    (t, _) <- expectAndGetDefinedSymbolM pos ident
    expectFunctionTypeM pos t
    let (ITFun funArgTs retT) = t
    appArgTs <- mapM evalM args
    expectMatchingArgsM pos ident funArgTs appArgTs
    return (retT, Imm)

  evalM (ELambda pos args ret block) = do
    expectUniqueArgumentsM args $ ArgumentRedeclarationE pos
    let funT@(ITFun argTs retT) = convertType (args, ret)
    let argIds = map getArgIdent args
    checkFunctionBodyM pos retT (zip argIds argTs) block
    return (funT, Imm)

checkFunctionBodyM :: Checker a => BNFC'Position -> InternalType -> [(Ident, ValueType)] -> a -> CheckerM a
checkFunctionBodyM pos retT args block = doNestedChecking $ do
  modify $ addTypes args
  doNestedChecking (do
    block' <- checkM (Just retT) block
    expectReturnOccuredM pos
    return block')

evalSimpleExprs :: BNFC'Position -> [Expr] -> [InternalType] -> InternalType -> CheckerM ValueType
evalSimpleExprs pos exps allowedTs retT = do
  ts <- mapM evalM exps
  let expTs = map fst ts
  expectSimpleExprsM pos expTs allowedTs
  return (retT, Imm)
