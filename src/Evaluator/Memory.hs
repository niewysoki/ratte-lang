module Evaluator.Memory (Value(..), Memory(..), emptyMemory, getValue, putValue, updateValue, retId, entrypointId, putEnv) where

import qualified Data.Map         as M
import           Generated.Syntax

data Value
  = ValEmpty
  | ValVoid
  | ValInt Integer
  | ValBool Bool
  | ValString String
  | ValFunction [Arg] Block Env

instance Eq Value where
  ValInt x == ValInt x' = x == x'
  ValBool x == ValBool x' = x == x'
  ValString x == ValString x' = x == x'
  _ == _ = False

type Loc = Int

newtype Env = Env {_env :: M.Map Ident Loc}

data Store = Str
  { _store   :: M.Map Loc Value
  , _nextLoc :: Loc
  }

data Memory = Mem
  { env   :: Env
  , store :: Store
  }

emptyMemory :: Memory
emptyMemory = Mem emptyEnv emptyStore where
  emptyEnv = Env M.empty :: Env
  emptyStore = Str M.empty 0 :: Store

retId = Ident "return"
entrypointId = Ident "Main"

getValue :: Ident -> Memory -> Value
getValue id mem = (_store . store $ mem) M.! getLoc id mem

getLoc :: Ident -> Memory -> Loc
getLoc id mem = (_env . env $ mem) M.! id

putValue :: Ident -> Value -> Memory -> Memory
putValue id val mem = undefined

updateValue :: Ident -> Value -> Memory -> Memory
updateValue id val mem = undefined

putEnv :: Env -> Memory -> Memory
putEnv env' mem = mem {env = env'}
