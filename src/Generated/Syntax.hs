-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The abstract syntax of language Ratte.

module Generated.Syntax where

import Prelude (Integer, String)
import qualified Prelude as C
  ( Eq, Ord, Show, Read
  , Functor, Foldable, Traversable
  , Int, Maybe(..)
  )
import qualified Data.String

type Program = Program' BNFC'Position
data Program' a = PProgram a [Init' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Init = Init' BNFC'Position
data Init' a
    = IFn a Ident [Arg' a] (Type' a) (Block' a)
    | IVar a Ident (Type' a) (Expr' a)
    | IConst a Ident (Type' a) (Expr' a)
    | IVarInf a Ident (Expr' a)
    | IConstInf a Ident (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Arg = Arg' BNFC'Position
data Arg' a = IArg a Ident (Type' a) | IArgMut a Ident (Type' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Block = Block' BNFC'Position
data Block' a = SBlock a [Stmt' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Stmt = Stmt' BNFC'Position
data Stmt' a
    = SEmpty a
    | SBStmt a (Block' a)
    | SInit a (Init' a)
    | SAss a Ident (Expr' a)
    | SIncr a Ident
    | SDecr a Ident
    | SRet a (Expr' a)
    | SVRet a
    | SCond a (Expr' a) (Block' a)
    | SCondElse a (Expr' a) (Block' a) (Block' a)
    | SWhile a (Expr' a) (Block' a)
    | SExp a (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Type = Type' BNFC'Position
data Type' a
    = TInt a
    | TStr a
    | TBool a
    | TVoid a
    | TFun a [ArgType' a] (Type' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type ArgType = ArgType' BNFC'Position
data ArgType' a = ATArg a (Type' a) | ATArgMut a (Type' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Expr = Expr' BNFC'Position
data Expr' a
    = EVar a Ident
    | ELitInt a Integer
    | ELitTrue a
    | ELitFalse a
    | EApp a Ident [Expr' a]
    | EString a String
    | ENeg a (Expr' a)
    | ENot a (Expr' a)
    | EMul a (Expr' a) (MulOp' a) (Expr' a)
    | EAdd a (Expr' a) (AddOp' a) (Expr' a)
    | ERel a (Expr' a) (RelOp' a) (Expr' a)
    | EAnd a (Expr' a) (Expr' a)
    | EOr a (Expr' a) (Expr' a)
    | ELambda a [Arg' a] (Type' a) (Block' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type AddOp = AddOp' BNFC'Position
data AddOp' a = OPlus a | OMinus a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type MulOp = MulOp' BNFC'Position
data MulOp' a = OTimes a | ODiv a | OMod a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type RelOp = RelOp' BNFC'Position
data RelOp' a = OLTH a | OLE a | OGTH a | OGE a | OEQU a | ONE a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

-- | Start position (line, column) of something.

type BNFC'Position = C.Maybe (C.Int, C.Int)

pattern BNFC'NoPosition :: BNFC'Position
pattern BNFC'NoPosition = C.Nothing

pattern BNFC'Position :: C.Int -> C.Int -> BNFC'Position
pattern BNFC'Position line col = C.Just (line, col)

-- | Get the start position of something.

class HasPosition a where
  hasPosition :: a -> BNFC'Position

instance HasPosition Program where
  hasPosition = \case
    PProgram p _ -> p

instance HasPosition Init where
  hasPosition = \case
    IFn p _ _ _ _ -> p
    IVar p _ _ _ -> p
    IConst p _ _ _ -> p
    IVarInf p _ _ -> p
    IConstInf p _ _ -> p

instance HasPosition Arg where
  hasPosition = \case
    IArg p _ _ -> p
    IArgMut p _ _ -> p

instance HasPosition Block where
  hasPosition = \case
    SBlock p _ -> p

instance HasPosition Stmt where
  hasPosition = \case
    SEmpty p -> p
    SBStmt p _ -> p
    SInit p _ -> p
    SAss p _ _ -> p
    SIncr p _ -> p
    SDecr p _ -> p
    SRet p _ -> p
    SVRet p -> p
    SCond p _ _ -> p
    SCondElse p _ _ _ -> p
    SWhile p _ _ -> p
    SExp p _ -> p

instance HasPosition Type where
  hasPosition = \case
    TInt p -> p
    TStr p -> p
    TBool p -> p
    TVoid p -> p
    TFun p _ _ -> p

instance HasPosition ArgType where
  hasPosition = \case
    ATArg p _ -> p
    ATArgMut p _ -> p

instance HasPosition Expr where
  hasPosition = \case
    EVar p _ -> p
    ELitInt p _ -> p
    ELitTrue p -> p
    ELitFalse p -> p
    EApp p _ _ -> p
    EString p _ -> p
    ENeg p _ -> p
    ENot p _ -> p
    EMul p _ _ _ -> p
    EAdd p _ _ _ -> p
    ERel p _ _ _ -> p
    EAnd p _ _ -> p
    EOr p _ _ -> p
    ELambda p _ _ _ -> p

instance HasPosition AddOp where
  hasPosition = \case
    OPlus p -> p
    OMinus p -> p

instance HasPosition MulOp where
  hasPosition = \case
    OTimes p -> p
    ODiv p -> p
    OMod p -> p

instance HasPosition RelOp where
  hasPosition = \case
    OLTH p -> p
    OLE p -> p
    OGTH p -> p
    OGE p -> p
    OEQU p -> p
    ONE p -> p

