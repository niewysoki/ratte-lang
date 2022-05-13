{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Types
  ( ValueType
  , Mutability(..)
  , InternalType(..)
  , CheckAssign(..)
  , Typing(..)
  ) where
import           Data.List        (intercalate)
import           Generated.Syntax

type ValueType = (InternalType, Mutability)

data Mutability = Imm | Mut deriving Eq

data InternalType
  = ITEmpty
  | ITVoid
  | ITInt
  | ITStr
  | ITBool
  | ITFun [ValueType] InternalType
  deriving Eq

instance Show InternalType where
  show ITVoid = "Void"
  show ITInt = "Int"
  show ITStr = "Str"
  show ITBool = "Bool"
  show (ITFun argTs retT) = "(" ++ showArgs argTs ++ ") -> " ++ show retT where
    showArgs = intercalate ", " . map showArg
    showArg (t, Mut) = "mut " ++ show t
    showArg (t, _)   = show t
  show _ = ""

instance Show Mutability where
  show Mut = "mut "
  show Imm = ""

class Typing a where
  convertType :: a -> InternalType

instance Typing Type where
  convertType (TInt _) = ITInt
  convertType (TStr _) = ITStr
  convertType (TBool _) = ITBool
  convertType (TVoid _) = ITVoid
  convertType (TFun _ args ret) = ITFun argTs (convertType ret) where
    argTs =  zip <$> map convertType <*> map getMutability $ args
    getMutability (ATArg _ _)    = Imm
    getMutability (ATArgMut _ _) = Mut

instance Typing ArgType where
  convertType (ATArg _ arg)    = convertType arg
  convertType (ATArgMut _ arg) = convertType arg

instance Typing Arg where
  convertType (IArg _ _ arg)    = convertType arg
  convertType (IArgMut _ _ arg) = convertType arg

instance Typing ([Arg], Type) where
  convertType (args, ret) = ITFun argTs (convertType ret) where
    argTs = zip <$> map convertType <*> map getMutability $ args
    getMutability IArg {}    = Imm
    getMutability IArgMut {} = Mut

class CheckAssign a where
  canAssign :: a -> a -> Bool

instance CheckAssign ValueType where
  canAssign (t, mut) (t', mut') = canAssign t t' && canAssign mut mut'

instance CheckAssign Mutability where
  canAssign Mut Imm = False
  canAssign _ _     = True

instance CheckAssign InternalType where
  canAssign (ITFun argTs retT) (ITFun argTs' retT') = canAssign retT' retT && all (== True) (zipWith canAssign argTs' argTs)
  canAssign t t' = t == t'
