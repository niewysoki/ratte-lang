{-# LANGUAGE FlexibleInstances #-}
module Evaluator.Evaluator (eval) where
import           Common.BuiltIn       (evalBuiltIn, isBuiltIn)
import           Control.Monad        (void, when)
import           Control.Monad.Except (MonadError (throwError), runExceptT)
import           Control.Monad.State  (evalStateT, gets, modify)
import           Data.List
import           Data.Map
import           Evaluator.Exceptions
import           Evaluator.Memory
import           Evaluator.Monads
import           Generated.Syntax

eval :: Program -> IO (Either RuntimeException ())
eval = runExceptT . flip evalStateT emptyMemory . evalM

instance Eval Program where
  evalM (PProgram pos inits) = do
    mapM_ evalM inits
    void . evalE $ EApp pos entrypointId []

instance Eval Init where
  evalM (IFn _ ident args _ block) = do
    fun <- gets $ ValFunction args block . getEnv
    modify $ putValue ident fun

  evalM (IVarInf _ ident exp) = do
    val <- evalE exp
    modify $ putValue ident val

  evalM (IVar pos _ _ _) = throwError $ UnknownE pos
  evalM (IConst pos _ _ _) = throwError $ UnknownE pos
  evalM (IConstInf pos _ _) = throwError $ UnknownE pos

instance Eval Block where
  evalM (SBlock _ stmts) = mapM_ evalM stmts

instance Eval Stmt where
  evalM (SEmpty _) = return ()
  evalM (SBStmt _ block) = guardReturn . protectEnv $ evalM block
  evalM (SInit _ init) = guardReturn $ evalM init

  evalM (SAss _ ident exp) = guardReturn $ do
    val <- evalE exp
    modify $ updateValue ident val

  evalM (SIncr _ ident) = guardReturn $ do
    (ValInt x) <- gets $ getValue ident
    modify $ updateValue ident (ValInt $ x + 1)

  evalM (SDecr _ ident) = guardReturn $ do
    (ValInt x) <- gets $ getValue ident
    modify $ updateValue ident (ValInt $ x - 1)

  evalM (SRet _ exp) = guardReturn $ do
    val <- evalE exp
    modify $ updateValue retId val

  evalM (SVRet _) = guardReturn . modify $ updateValue retId ValVoid

  evalM (SCond _ cond block) = guardReturn $ do
    (ValBool x) <- evalE cond
    protectEnv $ when x $ evalM block

  evalM (SCondElse _ cond block block') = guardReturn $ do
    (ValBool x) <- evalE cond
    protectEnv $ if x then evalM block else evalM block'

  evalM loop@(SWhile _ cond block) = guardReturn $ do
    (ValBool x) <- evalE cond
    when x $ do
      protectEnv (evalM block)
      evalM loop

  evalM (SExp _ exp) = guardReturn . void $ evalE exp

evalE :: Expr -> EvalValueM
evalE (ELitInt _ n) = return $ ValInt n
evalE (ELitTrue _)  = return $ ValBool True
evalE (ELitFalse _) = return $ ValBool False
evalE (EString _ s) = return $ ValStr s

evalE (ENeg _ exp)  = do
  (ValInt x) <- evalE exp
  return . ValInt $ negate x

evalE (ENot _ exp)  = do
  (ValBool x) <- evalE exp
  return . ValBool $ not x

evalE (ERel _ exp1 (OLTH _) exp2)   = evalIntToBoolOp (<) exp1 exp2
evalE (ERel _ exp1 (OLE _) exp2)    = evalIntToBoolOp (<=) exp1 exp2
evalE (ERel _ exp1 (OGTH _) exp2)   = evalIntToBoolOp (>) exp1 exp2
evalE (ERel _ exp1 (OGE _) exp2)    = evalIntToBoolOp (>=) exp1 exp2
evalE (EAnd _ exp1 exp2)            = evalBoolToBoolOp (&&) exp1 exp2
evalE (EOr _ exp1 exp2)             = evalBoolToBoolOp (||) exp1 exp2
evalE (ERel _ exp1 (ONE _) exp2)    = evalValToBoolOp (/=) exp1 exp2
evalE (ERel _ exp1 (OEQU _) exp2)   = evalValToBoolOp (==) exp1 exp2
evalE (EAdd _ exp1 (OPlus _) exp2)  = evalIntToIntOp (+) exp1 exp2
evalE (EAdd _ exp1 (OMinus _) exp2) = evalIntToIntOp (-) exp1 exp2
evalE (EMul _ exp1 (OTimes _) exp2) = evalIntToIntOp (*) exp1 exp2
evalE (EMul _ exp1 (OMod _) exp2)   = evalIntToIntOp mod exp1 exp2
evalE (EVar _ name)                 = gets (getValue name)
evalE (ELambda _ args _ block)      = gets (ValFunction args block . getEnv)
evalE (EMul p exp1 (ODiv _) exp2)   = do
  (ValInt x1) <- evalE exp1
  (ValInt x2) <- evalE exp2
  when (x2 == 0) $ throwError $ DivideByZeroE p
  return . ValInt $ div x1 x2

evalE (EApp _ ident argExps) = guardBuiltIn argExps ident . protectEnv $ do
  argVals <- mapM evalE argExps
  argSrcs <- gets $ mapM getArgLoc argExps
  fun@(ValFunction funArgs funBlock funEnv) <- gets $ getValue ident
  modify $ putEnv funEnv
  modify $ putValue ident fun
  modify $ putValue retId ValEmpty
  modify $ putArgValsLocs $ zip3 funArgs argVals argSrcs
  evalM funBlock
  gets $ getValue retId

evalValToBoolOp :: (Value -> Value -> Bool) -> Expr -> Expr -> EvalValueM
evalValToBoolOp op exp1 exp2 = do
  val1 <- evalE exp1
  val2 <- evalE exp2
  return . ValBool $ op val1 val2

evalIntToIntOp :: (Integer -> Integer -> Integer) -> Expr -> Expr -> EvalValueM
evalIntToIntOp op exp1 exp2 = do
  (ValInt x1) <- evalE exp1
  (ValInt x2) <- evalE exp2
  return . ValInt $ op x1 x2

evalIntToBoolOp :: (Integer -> Integer -> Bool) -> Expr -> Expr -> EvalValueM
evalIntToBoolOp op exp1 exp2 = do
  (ValInt x1) <- evalE exp1
  (ValInt x2) <- evalE exp2
  return . ValBool $ op x1 x2

evalBoolToBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> EvalValueM
evalBoolToBoolOp op exp1 exp2 = do
  (ValBool x1) <- evalE exp1
  (ValBool x2) <- evalE exp2
  return . ValBool $ op x1 x2

guardReturn :: EvalEmptyM -> EvalEmptyM
guardReturn computation = do
  ret <- gets $ (== ValEmpty) . getValue retId
  when ret computation

guardBuiltIn :: [Expr] -> Ident -> EvalValueM -> EvalValueM
guardBuiltIn argExps ident computation = do
  unshadowed <- gets $ not . hasValue ident
  if isBuiltIn ident && unshadowed
  then mapM evalE argExps >>= evalBuiltIn ident
  else computation

protectEnv :: EvalM a -> EvalM a
protectEnv computation = do
  env <- gets getEnv
  val <- computation
  modify $ putEnv env
  return val
