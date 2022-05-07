{-# LANGUAGE RecordWildCards #-}
module Typechecker.Persistence where

import           Data.Map          as M (Map, empty, insert, lookup)
import           Generated.Syntax  (Ident)
import           Typechecker.Types (IType)

data Env = Env
  { _env :: M.Map Ident IType
  , _ret :: Bool
  }

emptyEnv :: Env
emptyEnv = Env M.empty False
 
putType :: Ident -> IType -> Env -> Env
putType id itype Env{..} = Env {_env = M.insert id itype _env, _ret = _ret}

getType :: Ident -> Env -> Maybe IType
getType id = M.lookup id . _env

setReturn :: Env -> Env
setReturn env = env {_ret = True}

hasReturn :: Env -> Bool
hasReturn = _ret