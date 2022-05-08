{-# LANGUAGE FlexibleInstances #-}
module Evaluator.Evaluator (eval) where
import Control.Monad.Except
import           Control.Monad.State
import           Evaluator.Exceptions
import           Evaluator.Memory
import           Evaluator.Monads
import           Generated.Syntax
import Common.BuiltIn

eval :: Program -> IO (Either RuntimeException Value)
eval = runExceptT . flip evalStateT emptyMemory . evalM

instance Eval Program where
  evalM (PProgram pos inits) = do
    mapM_ evalM inits
    evalM $ EApp pos entrypointId []

instance Eval Init where
  evalM (IFn _ ident args _ block) = do
    fun <- gets $ ValFunction args block . getEnv
    modify $ putValue ident fun
    return ValEmpty

  evalM (IVarMut _ ident _ exp) = do
    val <- evalM exp
    modify $ putValue ident val
    return ValEmpty

  evalM (IVar _ ident _ exp) = do
    val <- evalM exp
    modify $ putValue ident val
    return ValEmpty

instance Eval Block where
  evalM (SBlock _ stmts) = do
    mapM_ evalM stmts
    return ValEmpty

instance Eval Stmt where
  evalM (SEmpty _) = guardReturn $ return ValEmpty
  evalM (SBStmt _ block) = guardReturn . protectEnv $ evalM block
  evalM (SInit _ init) = guardReturn $ evalM init

  evalM (SAss _ ident exp) = guardReturn $ do
    val <- evalM exp
    modify $ updateValue ident val
    return ValEmpty

  evalM (SIncr _ ident) = guardReturn $ do
    (ValInt x) <- gets $ getValue ident
    modify $ updateValue ident (ValInt $ x + 1)
    return ValEmpty

  evalM (SDecr _ ident) = guardReturn $ do
    (ValInt x) <- gets $ getValue ident
    modify $ updateValue ident (ValInt $ x - 1)
    return ValEmpty

  evalM (SRet _ exp) = guardReturn $ do
    val <- evalM exp
    modify $ updateValue retId val
    return ValEmpty

  evalM (SVRet _) = guardReturn $ do
    modify $ updateValue retId ValVoid
    return ValEmpty

  evalM (SCond _ cond block) = guardReturn $ do
    (ValBool x) <- evalM cond
    protectEnv $ if x then evalM block else return ValEmpty

  evalM (SCondElse _ cond block block') = guardReturn $ do
    (ValBool x) <- evalM cond
    protectEnv $ if x then evalM block else evalM block'

  evalM loop@(SWhile _ cond block) = guardReturn $ do
    (ValBool x) <- evalM cond
    if x then do
      protectEnv (evalM block)
      evalM loop
    else return ValEmpty

  evalM (SExp _ exp) = guardReturn $ evalM exp

instance Eval Expr where
  evalM (ELitInt _ n) = return $ ValInt n
  evalM (ELitTrue _)  = return $ ValBool True
  evalM (ELitFalse _) = return $ ValBool False
  evalM (EString _ s) = return $ ValStr s

  evalM (ENeg _ exp)  = do
    (ValInt x) <- evalM exp
    return . ValInt $ negate x
  evalM (ENot _ exp)  = do
    (ValBool x) <- evalM exp
    return . ValBool $ not x

  evalM (ERel _ exp1 (OLTH _) exp2)   = evalIntToBoolOp (<) exp1 exp2
  evalM (ERel _ exp1 (OLE _) exp2)    = evalIntToBoolOp (<=) exp1 exp2
  evalM (ERel _ exp1 (OGTH _) exp2)   = evalIntToBoolOp (>) exp1 exp2
  evalM (ERel _ exp1 (OGE _) exp2)    = evalIntToBoolOp (>=) exp1 exp2
  evalM (EAnd _ exp1 exp2)            = evalBoolToBoolOp (&&) exp1 exp2
  evalM (EOr _ exp1 exp2)             = evalBoolToBoolOp (||) exp1 exp2
  evalM (ERel _ exp1 (ONE _) exp2)    = evalValToBoolOp (/=) exp1 exp2
  evalM (ERel _ exp1 (OEQU _) exp2)   = evalValToBoolOp (==) exp1 exp2
  evalM (EAdd _ exp1 (OPlus _) exp2)  = evalIntToIntOp (+) exp1 exp2
  evalM (EAdd _ exp1 (OMinus _) exp2) = evalIntToIntOp (-) exp1 exp2
  evalM (EMul _ exp1 (OTimes _) exp2) = evalIntToIntOp (*) exp1 exp2
  evalM (EMul _ exp1 (OMod _) exp2)   = evalIntToIntOp mod exp1 exp2
  evalM (EVar _ name)                 = gets (getValue name)
  evalM (ELambda _ args _ block)      = gets (ValFunction args block . getEnv)
  evalM (EMul p exp1 (ODiv _) exp2)   = do
    (ValInt x2) <- evalM exp2
    if x2 == 0 then throwError $ DivideByZeroE p else evalIntToIntOp quot exp1 exp2

  evalM (EApp _ ident argExps) = guardBuiltIn argExps ident . protectEnv $ do
    argVals <- mapM evalM argExps
    argSrcs <- gets $ mapM getArgLoc argExps
    fun@(ValFunction funArgs funBlock funEnv) <- gets $ getValue ident
    modify $ putEnv funEnv
    modify $ putValue ident fun
    modify $ putValue retId ValEmpty
    modify $ putArgValsLocs $ zip3 funArgs argVals argSrcs
    evalM funBlock
    gets $ getValue retId

evalValToBoolOp :: (Value -> Value -> Bool) -> Expr -> Expr -> EvalM
evalValToBoolOp op exp1 exp2 = do
  val1 <- evalM exp1
  val2 <- evalM exp2
  return . ValBool $ op val1 val2

evalIntToIntOp :: (Integer -> Integer -> Integer) -> Expr -> Expr -> EvalM
evalIntToIntOp op exp1 exp2 = do
  (ValInt x1) <- evalM exp1
  (ValInt x2) <- evalM exp2
  return . ValInt $ op x1 x2

evalIntToBoolOp :: (Integer -> Integer -> Bool) -> Expr -> Expr -> EvalM
evalIntToBoolOp op exp1 exp2 = do
  (ValInt x1) <- evalM exp1
  (ValInt x2) <- evalM exp2
  return . ValBool $ op x1 x2

evalBoolToBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> EvalM
evalBoolToBoolOp op exp1 exp2 = do
  (ValBool x1) <- evalM exp1
  (ValBool x2) <- evalM exp2
  return . ValBool $ op x1 x2

guardReturn :: EvalM -> EvalM
guardReturn computation = do
  ret <- gets $ (== ValEmpty) . getValue retId
  if ret then computation else return ValEmpty

guardBuiltIn :: [Expr] -> Ident -> EvalM -> EvalM
guardBuiltIn argExps ident computation = if isBuiltIn ident
  then mapM evalM argExps >>= evalBuiltIn ident
  else computation
 
protectEnv :: EvalM -> EvalM
protectEnv computation = do
  env <- gets getEnv
  val <- computation
  modify $ putEnv env
  return val
