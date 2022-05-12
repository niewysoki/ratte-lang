{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Types where
import Data.List
import Generated.Syntax

type ValueType = (InternalType, Mutability)

data Mutability = Imm | Mut | Any deriving Eq

data InternalType
  = ITEmpty
  | ITVoid
  | ITInt
  | ITStr
  | ITBool
  | ITFunction [ValueType] InternalType
  deriving Eq

instance Show InternalType where
  show ITVoid = "Void"
  show ITInt = "Int"
  show ITStr = "Str"
  show ITBool = "Bool"
  show (ITFunction argTs retT) = "(" ++ showArgs argTs ++ ") -> " ++ show retT where
    showArgs = intercalate ", " . map showArg
    showArg (t, Mut) = "mut " ++ show t
    showArg (t, _) = show t
  show _ = ""

class Typing a where
  convertType :: a -> InternalType

instance Typing Type where
  convertType (TInt _) = ITInt
  convertType (TStr _) = ITStr
  convertType (TBool _) = ITBool
  convertType (TVoid _) = ITVoid
  convertType (TFun _ args ret) = ITFunction argTs (convertType ret) where
    argTs =  zip <$> map convertType <*> map (const Any) $ args

instance Typing Arg where
  convertType (IArg _ _ arg) = convertType arg
  convertType (IArgMut _ _ arg) = convertType arg

instance Typing ([Arg], Type) where
  convertType (args, ret) = ITFunction argTs (convertType ret) where
    argTs = zip <$> map convertType <*> map getMutability $ args
    getMutability IArg {} = Imm
    getMutability IArgMut {} = Mut
