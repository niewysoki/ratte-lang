{-# LANGUAGE FlexibleInstances #-}

module Typechecker.Typechecker (typecheck) where

import           Control.Monad.Except
import           Control.Monad.State
import           Generated.Syntax
import           Typechecker.Exceptions
import           Typechecker.Monads
import           Typechecker.Persistence
import           Typechecker.Types

typecheck :: Program -> Either TypecheckingException Program
typecheck p = Right p
-- typecheck p = runExcept (evalStateT (checkM Nothing p) emptyEnv)

instance Checker Program where
  checkM _ (PProgram position inits) = undefined

instance Checker Init where
  checkM _ (IFn pos id args ret block) = do
    let fnT = undefined
    let retT = undefined
    let argsTs = undefined

    modify $ putType id fnT
    env <- get
    -- modify $ putTypes argsTs
    checkM (Just retT) block
    env' <- get
    let hasret = hasReturn env'

    put env

  checkM _ (IVar pos id t exp) = do
    return ()
  checkM _ (IVarMut pos id t exp) = do
    return ()

instance Checker Block where
  checkM retT (SBlock _ stmts) = mapM_ (checkM retT) stmts

instance Checker Stmt where
  checkM = undefined

-- instance Getter Expr where
--   getM (ELitInt _ _) = return $ IInt
--   getM (ELitTrue _) = return $ IBool
--   getM (ELitFalse _) = return $ IBool Imm
--   getM (EString _ _) = return $ IStr Imm

--   getM (ENeg pos exp) = undefined

--   getM _ = undefined






assertTypeOrThrowM expectedType actualType = assertOrThrowM isValidType
  where
    isValidType = expectedType == actualType

assertOrThrowM :: Bool -> TypecheckingException -> CheckerM
assertOrThrowM True _          = pure ()
assertOrThrowM False exception = throwError exception



