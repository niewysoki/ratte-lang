{-# LANGUAGE FlexibleInstances #-}
module Evaluator.Evaluator (eval) where
import           Control.Monad.Except
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
  evalM (IFn _ id args _ block) = do
    fun <- gets $ ValFunction args block . env
    modify $ putValue id fun
    return ValEmpty

  evalM (IVarMut _ id _ exp) = do
    val <- evalM exp
    modify $ putValue id val
    return ValEmpty

  evalM (IVar _ id _ exp) = do
    val <- evalM exp
    modify $ putValue id val
    return ValEmpty

instance Eval Block where
  evalM (SBlock _ stmts) = do
    mapM_ evalM stmts
    return ValEmpty

instance Eval Stmt where
  evalM (SEmpty _) = guardReturn $ return ValEmpty
  evalM (SBStmt _ block) = guardReturn . preserveEnv $ evalM block
  evalM (SInit _ init) = guardReturn $ evalM init

  evalM (SAss _ id exp) = guardReturn $ do
    val <- evalM exp
    modify $ updateValue id val
    return ValEmpty

  evalM (SIncr _ id) = guardReturn $ do
    (ValInt x) <- gets $ getValue id
    modify $ updateValue id (ValInt $ x + 1)
    return ValEmpty

  evalM (SDecr _ id) = guardReturn $ do
    (ValInt x) <- gets $ getValue id
    modify $ updateValue id (ValInt $ x - 1)
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
    preserveEnv $ if x then evalM block else return ValEmpty

  evalM (SCondElse _ cond block block') = guardReturn $ do
    (ValBool x) <- evalM cond
    preserveEnv . evalM $ if x then block else block'

  evalM loop@(SWhile _ cond block) = guardReturn $ do
    (ValBool x) <- evalM cond
    preserveEnv $ if x then do evalM block >> evalM loop else return ValEmpty

  evalM (SExp _ exp) = guardReturn $ evalM exp

instance Eval Expr where
  evalM (ELitInt _ n) = return $ ValInt n
  evalM (ELitTrue _)  = return $ ValBool True
  evalM (ELitFalse _) = return $ ValBool False
  evalM (EString _ s) = return $ ValString s

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
  evalM (ELambda _ args _ block)      = gets (ValFunction args block . env)
  evalM (EMul p exp1 (ODiv _) exp2)   = do
    (ValInt x2) <- evalM exp2
    if x2 == 0 then throwError $ DivideByZeroE p else evalIntToIntOp quot exp1 exp2

  evalM (EApp _ id argExps) = guardBuiltIn argExps id $ do
    mem <- get
    argVals <- mapM evalM argExps
    -- argLocs <-

    let env' = env mem
    let fun@(ValFunction funArgs funBlock funEnv) = getValue id mem
    modify $ putEnv funEnv
    modify $ putValue id fun
    modify $ putValue retId ValEmpty

    --mapM_

    evalM funBlock

    let val = getValue retId mem
    modify $ putEnv env'
    return val


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
  if ret then return ValEmpty else computation

guardBuiltIn :: [Expr] -> Ident -> EvalM -> EvalM
guardBuiltIn argExps id computation = if isBuiltIn id
  then mapM evalM argExps >>= evalBuiltIn id
  else computation

preserveEnv :: EvalM -> EvalM
preserveEnv computation = do
  env' <- gets env
  computation
  modify $ putEnv env'
  return ValEmpty
