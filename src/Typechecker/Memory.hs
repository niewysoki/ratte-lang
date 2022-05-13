module Typechecker.Memory
  ( Memory
  , emptyMemory
  , getType
  , addType
  , addTypes
  , setOuterEnv
  , setReturn
  , hasReturn
  , hasSymbolInCurrentContext
  ) where

import           Common.BuiltIn    (builtInFuncTypes)
import           Data.Map          as M (Map, empty, fromList, insert, lookup,
                                         member, union)
import           Generated.Syntax
import           Typechecker.Types

type Env = M.Map Ident ValueType

data Memory = Mem
  { _env       :: Env
  , _outer_env :: Env
  , _ret       :: Bool
  }

emptyMemory :: Memory
emptyMemory = Mem
  { _env = M.empty
  , _outer_env = fromList builtInFuncTypes
  , _ret = False
  }

getType :: Ident -> Memory -> Maybe ValueType
getType = (. getEnvUnion) . M.lookup

addType :: Ident -> ValueType -> Memory -> Memory
addType ident t mem = mem {_env = M.insert ident t (_env mem)}

addTypes :: [(Ident, ValueType)] -> Memory -> Memory
addTypes ts mem = mem {_env = M.union (fromList ts) (_env mem)}

setOuterEnv :: Memory -> Memory
setOuterEnv mem = Mem
  { _env = M.empty
  , _outer_env = getEnvUnion mem
  , _ret = _ret mem
  }

setReturn :: Memory -> Memory
setReturn mem = mem {_ret = True}

hasReturn :: Memory -> Bool
hasReturn = _ret

hasSymbolInCurrentContext :: Ident -> Memory -> Bool
hasSymbolInCurrentContext = (. _env) . M.member

getEnvUnion :: Memory -> Env
getEnvUnion = M.union <$> _env <*> _outer_env
