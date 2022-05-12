module Typechecker.Exceptions (TypeCheckingException(..)) where
import Generated.Syntax
import Typechecker.Types

type Pos = BNFC'Position

data TypeCheckingException
  = InvalidTypeE Pos InternalType InternalType
  | UndefinedSymbolE Pos Ident
  | NotCallableE Pos InternalType
  | ArgumentCountMismatchE Pos Ident Int Int
  | ArgumentTypesMismatchE Pos InternalType InternalType
  | ArgumentConstViolationE Pos InternalType
  | TypeMismatchE Pos InternalType InternalType
  | ConstViolationE Pos InternalType
  | RedeclarationE Pos Ident Pos
  | ReturnOutOfScopeE Pos
  | ReturnTypeMismatchE Pos InternalType InternalType
  | NoReturnStatementE Pos
  | ArgumentRedefinitionE Pos
  | RedefinitionE Pos Ident deriving Show
