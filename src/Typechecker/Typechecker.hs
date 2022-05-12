{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Typechecker (typecheck) where
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
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
    mapM_ (checkM Nothing) inits
    checkM Nothing $ SExp pos $ EApp pos (Ident "Main") []

instance Checker Init where
  checkM _ (IFn pos id args ret block) = do
    Comm.expectUniqueArgumentsM args $ ArgumentRedefinitionE pos
    let funT@(ITFunction argTs retT) = convertType (args, ret)
    let argIds = map getArgIdent args
    modify $ addType id (funT, Imm)
    mem <- get
    modify $ addTypes $ zip argIds argTs
    doNestedChecking (do
      checkM (Just retT) block
      Chck.expectReturnOccuredM pos)
    put mem

  checkM _ (IVar pos id t exp)    = checkVarInit pos id t exp Imm
  checkM _ (IVarMut pos id t exp) = checkVarInit pos id t exp Mut

instance Checker Block where
  checkM retT (SBlock _ stmts) = mapM_ (checkM retT) stmts

instance Checker Stmt where
  checkM _ (SEmpty _)            = return ()
  checkM retT (SBStmt pos block) = doNestedChecking $ checkM retT block
  checkM retT (SInit _ init)     = checkM retT init

  checkM _ (SAss pos ident exp) = do
    (t, mut) <- Chck.expectAndGetDefinedSymbolM pos ident
    (t', _) <- eval exp
    Comm.assertM (t == t') (TypeMismatchE pos t t')
    Comm.assertM (mut == Mut) (ConstViolationE pos t)

  checkM _ (SIncr pos ident) = do
    (t, mut) <- Chck.expectAndGetDefinedSymbolM pos ident
    Comm.assertM (t == ITInt) (TypeMismatchE pos ITInt t)
    Comm.assertM (mut == Mut) (ConstViolationE pos t)

  checkM retT (SDecr pos ident) = checkM retT (SIncr pos ident)

  checkM Nothing (SRet pos _) = throwError $ ReturnOutOfScopeE pos
  checkM (Just retT) (SRet pos exp) = do
    (expT, _) <- eval exp
    Comm.assertM (expT == retT) (ReturnTypeMismatchE pos retT expT)
    modify setReturn

  checkM Nothing (SVRet pos) = throwError $ ReturnOutOfScopeE pos
  checkM (Just retT) (SVRet pos) = do
    Comm.assertM (retT == ITVoid) (ReturnTypeMismatchE pos ITVoid retT)
    modify setReturn

  checkM retT (SCond pos cond block) = do
    expectTypeM pos cond ITBool
    doNestedChecking $ checkM retT block

  checkM retT (SCondElse pos cond block block') = do
    expectTypeM pos cond ITBool
    doNestedChecking $ checkM retT block
    doNestedChecking $ checkM retT block'

  checkM retT (SWhile pos cond block) = do
    expectTypeM pos cond ITBool
    doNestedChecking $ checkM retT block

  checkM _ (SExp pos exp) = expectTypeM pos exp ITVoid

instance Eval Expr where
  evalM (ELitInt _ _)          = return (ITInt, Imm)
  evalM (ELitFalse _)          = return (ITBool, Imm)
  evalM (ELitTrue _)           = return (ITBool, Imm)
  evalM (EString _ _)          = return (ITStr, Imm)
  evalM (ENeg pos exp)         = mapM evalM [exp] >>= Eval.expectTypesM pos ITInt ITInt
  evalM (ENot pos exp)         = mapM evalM [exp] >>= Eval.expectTypesM pos ITBool ITBool
  evalM (EMul pos exp1 _ exp2) = mapM evalM [exp1, exp2] >>= Eval.expectTypesM pos ITInt ITInt
  evalM (EAdd pos exp1 _ exp2) = mapM evalM [exp1, exp2] >>= Eval.expectTypesM pos ITInt ITInt
  evalM (ERel pos exp1 _ exp2) = mapM evalM [exp1, exp2] >>= Eval.expectTypesM pos ITInt ITBool
  evalM (EAnd pos exp1 exp2)   = mapM evalM [exp1, exp2] >>= Eval.expectTypesM pos ITBool ITBool
  evalM (EOr pos exp1 exp2)    = mapM evalM [exp1, exp2] >>= Eval.expectTypesM pos ITBool ITBool
  evalM (EVar pos ident)       = Eval.expectAndGetDefinedSymbolM pos ident

  evalM (EApp pos ident args) = do
    (t, _) <- Eval.expectAndGetDefinedSymbolM pos ident
    Eval.expectFunctionTypeM pos t
    let (ITFunction funArgTs retT) = t
    appArgTs <- mapM evalM args
    Eval.expectMatchingArgsM pos ident funArgTs appArgTs
    return (retT, Imm)

  evalM (ELambda pos args ret block) = do
    Comm.expectUniqueArgumentsM args $ ArgumentRedefinitionE pos
    let funT@(ITFunction argTs retT) = convertType (args, ret)
    let argIds = map getArgIdent args
    --local  (addTypes (zip argIds argTs)) TODO
    return (funT, Imm)


eval :: Expr -> CheckerWithValueM
eval exp = do
  mem <- get
  case runExcept $ runReaderT (evalM exp) mem of
    Left tce -> throwError tce
    Right t  -> return t

expectTypeM :: BNFC'Position -> Expr -> InternalType -> CheckerM
expectTypeM pos cond t = do
  mem <- get
  (condT, _) <- eval cond
  Comm.assertM (condT == t) $ TypeMismatchE pos condT t

checkVarInit :: BNFC'Position -> Ident -> Type -> Expr -> Mutability -> CheckerM
checkVarInit pos id t exp mut = do
  Chck.expectUniqueOrShadowM pos id
  (expT, _) <- eval exp
  let varT = convertType t
  Comm.assertM (expT == varT) (TypeMismatchE pos varT expT)
  modify $ addType id (expT, mut)

doNestedChecking :: CheckerM -> CheckerM
doNestedChecking checking = do
  mem <- get
  modify setOuterEnv
  checking
  put mem
