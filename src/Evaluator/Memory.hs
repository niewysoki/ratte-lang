{-# LANGUAGE RecordWildCards #-}
module Evaluator.Memory (Value(..), Memory, emptyMemory, getValue, putValue, updateValue, retId, entrypointId, putEnv, getEnv, _store) where

import qualified Data.Map         as M
import           Generated.Syntax

data Value
  = ValEmpty
  | ValVoid
  | ValInt Integer
  | ValBool Bool
  | ValString String
  | ValFunction [Arg] Block Env deriving Show

instance Eq Value where
  ValInt x == ValInt x' = x == x'
  ValBool x == ValBool x' = x == x'
  ValString x == ValString x' = x == x'
  _ == _ = False

type Loc = Int
type Env = M.Map Ident Loc
type Store = M.Map Loc Value

data Memory = Mem
  { _env     :: Env
  , _store   :: Store
  , _nextLoc :: Loc
  } deriving Show

emptyMemory :: Memory
emptyMemory = Mem M.empty M.empty 0

retId, entrypointId :: Ident
retId = Ident "return"
entrypointId = Ident "Main"

getValue :: Ident -> Memory -> Value
getValue id mem = _store mem M.! getLoc id mem

putValue :: Ident -> Value -> Memory -> Memory
putValue id val Mem{..} = Mem
  { _env     = M.insert id _nextLoc _env
  , _store   = M.insert _nextLoc val _store
  , _nextLoc = _nextLoc + 1
  }

updateValue :: Ident -> Value -> Memory -> Memory
updateValue id val mem = mem {_store = store'} where
  store' = M.insert (getLoc id mem) val (_store mem)

putEnv :: Env -> Memory -> Memory
putEnv env' mem = mem {_env = env'}

getEnv :: Memory -> Env
getEnv = _env

getLoc :: Ident -> Memory -> Loc
getLoc id mem = _env mem M.! id
