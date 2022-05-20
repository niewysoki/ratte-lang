module Typechecker.Utils
  ( assertM
  , doNestedChecking
  , doNestedCheckingWithReturn
  , initGetIdent
  , initGetPos
  , getArgIdent
  , validateFunArgsNames
  , validateFunArgsTypes
  , initsEq
  ) where
import           Control.Monad.Except (MonadError (throwError))
import           Control.Monad.State  (MonadState (..), gets, modify)
import           Data.List            (nub)
import           Generated.Syntax
import           Typechecker.Memory
import           Typechecker.Monads

doNestedChecking :: CheckerM a -> CheckerM a
doNestedChecking = (fst <$>) . doNestedCheckingWithReturn

doNestedCheckingWithReturn :: CheckerM a -> CheckerM (a, Bool)
doNestedCheckingWithReturn checking = do
  mem <- get
  modify setOuterEnv
  res <- checking
  ret <- gets hasReturn
  put mem
  return (res, ret)

assertM :: MonadError e m => Bool -> e -> m ()
assertM False ex = throwError ex
assertM _ _      = return ()

initGetIdent :: Init -> Ident
initGetIdent (IFn _ ident _ _ _)   = ident
initGetIdent (IConst _ ident _ _)  = ident
initGetIdent (IVar _ ident _ _)    = ident
initGetIdent (IVarInf _ ident _)   = ident
initGetIdent (IConstInf _ ident _) = ident

initGetPos :: Init -> BNFC'Position
initGetPos (IFn pos _ _ _ _)   = pos
initGetPos (IConst pos _ _ _)  = pos
initGetPos (IVar pos _ _ _)    = pos
initGetPos (IVarInf pos _ _)   = pos
initGetPos (IConstInf pos _ _) = pos

getArgIdent :: Arg -> Ident
getArgIdent (IArg _ id _)    = id
getArgIdent (IArgMut _ id _) = id

getArgType :: Arg -> Type
getArgType (IArg _ _ t)    = t
getArgType (IArgMut _ _ t) = t

getFunArgType :: ArgType -> Type
getFunArgType (ATArg _ t)    = t
getFunArgType (ATArgMut _ t) = t

validateFunArgsNames :: [Arg] -> Bool
validateFunArgsNames args = (==) <$> nub <*> id $ map getArgIdent args

validateFunArgsTypes :: [Arg] -> Bool
validateFunArgsTypes = all (nonVoidFunArg . getArgType)

nonVoidFunArg :: Type -> Bool
nonVoidFunArg (TFun _ args _) = nonVoidFunArgs args
nonVoidFunArg TVoid {}        = False
nonVoidFunArg _               = True

nonVoidFunArgs :: [ArgType] -> Bool
nonVoidFunArgs = all (nonVoidFunArg . getFunArgType)

initsEq :: Init -> Init -> Bool
initsEq i1 i2 = initGetIdent i1 == initGetIdent i2
