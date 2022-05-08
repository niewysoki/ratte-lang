module Typechecker.Memory where

import           Data.Map          as M
import           Generated.Syntax
import           Typechecker.Types 

type Env = M.Map Ident (InternalType, Mutability)

data Memory = Mem
  { _env :: Env
  , _prev_env :: Env
  , _ret :: Bool
  }

emptyMemory :: Memory
emptyMemory = Mem M.empty M.empty False

getType :: Ident -> Memory -> Maybe (InternalType, Mutability)
getType ident mem = M.lookup ident (_env mem)

getEnv :: Memory -> Env
getEnv = _env