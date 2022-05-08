module Typechecker.Common where
import Control.Monad.Except
import Typechecker.Types

assertM :: MonadError e m => Bool -> e -> m ()
assertM False ex = throwError ex
assertM _ _ = return ()

canAssign :: Mutability -> Mutability -> Bool
canAssign Imm Mut = False
canAssign _ _ = True