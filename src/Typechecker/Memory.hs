module Typechecker.Memory where

import           Data.Map          as M
import           Generated.Syntax
import           Typechecker.Types
import           Common.BuiltIn


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
getType ident = M.lookup ident . getEnvUnion

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
