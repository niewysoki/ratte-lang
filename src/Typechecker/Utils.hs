module Typechecker.Utils
  ( assertM
  , doNestedChecking
  , initGetIdent
  , initGetPos
  , getArgIdent
  , validateFunArgs
  , initsEq
  ) where
import           Control.Monad.Except (MonadError (throwError))
import           Control.Monad.State  (MonadState (..), modify)
import           Data.List            (nub)
import           Generated.Syntax
import           Typechecker.Memory
import           Typechecker.Monads

doNestedChecking :: CheckerM a -> CheckerM a
doNestedChecking checking = do
  mem <- get
  modify setOuterEnv
  ret <- checking
  put mem
  return ret

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

validateFunArgs :: [Arg] -> Bool
validateFunArgs args = (==) <$> nub <*> id $ map getArgIdent args

initsEq :: Init -> Init -> Bool
initsEq i1 i2 = initGetIdent i1 == initGetIdent i2