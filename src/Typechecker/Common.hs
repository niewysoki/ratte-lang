module Typechecker.Common
  ( assertM
  , expectUniqueArgumentsM
  , initGetIdent
  , initGetPos
  , getArgIdent
  ) where
import           Control.Monad.Except (MonadError (throwError))
import           Data.List            (nub)
import           Generated.Syntax

assertM :: MonadError e m => Bool -> e -> m ()
assertM False ex = throwError ex
assertM _ _      = return ()

expectUniqueArgumentsM :: MonadError e m => [Arg] -> e -> m ()
expectUniqueArgumentsM args ex = do
  let ret = validateFunArgs args
  assertM ret ex

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
