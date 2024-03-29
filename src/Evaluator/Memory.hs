{-# LANGUAGE RecordWildCards #-}
module Evaluator.Memory
  ( Value(..)
  , Memory
  , emptyMemory
  , retId
  , entrypointId
  , lambdaId
  , getValue
  , putValue
  , hasValue
  , updateValue
  , putEnv
  , getEnv
  , putArgVal
  , putArgValsLocs
  , getArgLoc
  )
  where

import           Control.Monad    (liftM2)
import qualified Data.Map         as M
import           Generated.Syntax

data Value
  = ValEmpty
  | ValVoid
  | ValInt Integer
  | ValBool Bool
  | ValStr String
  | ValFunction Ident [Arg] Block Env deriving Show

instance Eq Value where
  ValInt x == ValInt x' = x == x'
  ValBool x == ValBool x' = x == x'
  ValStr x == ValStr x' = x == x'
  ValEmpty == ValEmpty = True
  _ == _ = False

type Loc = Int
type Env = M.Map Ident Loc
type Store = M.Map Loc Value
data ArgSource = ArgLoc Loc | ArgVal

data Memory = Mem
  { _env     :: Env
  , _store   :: Store
  , _nextLoc :: Loc
  } deriving Show

emptyMemory :: Memory
emptyMemory = Mem M.empty M.empty 0

retId, entrypointId, lambdaId :: Ident
retId = Ident "return"
entrypointId = Ident "Main"
lambdaId = Ident ""

getValue :: Ident -> Memory -> Value
getValue = liftM2 (M.!) _store . getLoc

putValue :: Ident -> Value -> Memory -> Memory
putValue ident val Mem{..} = Mem
  { _env     = M.insert ident _nextLoc _env
  , _store   = M.insert _nextLoc val _store
  , _nextLoc = _nextLoc + 1
  }

updateValue :: Ident -> Value -> Memory -> Memory
updateValue ident val mem = mem {_store = store'} where
  store' = M.insert (getLoc ident mem) val (_store mem)

putEnv :: Env -> Memory -> Memory
putEnv env' mem = mem {_env = env'}

getEnv :: Memory -> Env
getEnv = _env

hasValue :: Ident -> Memory -> Bool
hasValue = (. _env) . M.member

putArgVal :: (Arg, Value, ArgSource) -> Memory -> Memory
putArgVal (IArg _ ident _, val, _)           = putValue ident val
putArgVal (IArgMut _ ident _, _, ArgLoc loc) = bindLoc ident loc where
  bindLoc ident loc mem = mem {_env = M.insert ident loc (_env mem)}
putArgVal _ = id

putArgValsLocs :: [(Arg, Value, ArgSource)] -> Memory -> Memory
putArgValsLocs = flip $ foldr putArgVal

getLoc :: Ident -> Memory -> Loc
getLoc = flip ((M.!) . _env)

getArgLoc :: Expr -> Memory -> ArgSource
getArgLoc (EVar _ name) = ArgLoc . getLoc name
getArgLoc _             = const ArgVal
