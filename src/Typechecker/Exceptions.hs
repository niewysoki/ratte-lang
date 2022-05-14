module Typechecker.Exceptions (TypeCheckingException(..)) where
import           Common.Utils
import           Generated.Syntax
import           Typechecker.Types

type Pos = BNFC'Position

data TypeCheckingException
  = UndefinedSymbolE Pos Ident
  | NotCallableE Pos InternalType
  | ArgumentCountMismatchE Pos Ident Int Int
  | ArgumentTypesMismatchE Pos ValueType ValueType
  | TypeMismatchE Pos InternalType InternalType
  | ConstViolationE Pos Ident InternalType
  | ReturnOutOfScopeE Pos
  | ReturnTypeMismatchE Pos InternalType InternalType
  | NoReturnStatementE Pos
  | ArgumentRedeclarationE Pos
  | RedeclarationE Pos Ident
  | VoidAssignmentE Pos
  | OpTypesUnequalE Pos InternalType InternalType

showE :: [String] -> String
showE msgs = unwords ("ERROR:" : msgs)

expectation :: String -> String -> String -> BNFC'Position -> [String]
expectation msg s s' pos = [msg, s, "but got", s', "instead, at", showP pos]

instance Show TypeCheckingException where
  show (UndefinedSymbolE pos id)            = showE ["undefined symbol", showI id, "at", showP pos]
  show (NotCallableE pos t)                 = showE $ expectation "not callable. Expected" "function type" (show t) pos
  show (ArgumentCountMismatchE pos id c c') = showE $ expectation ("argument count mismatch in invocation of " ++ showI id ++ ". Expected number of arguments") (show c) (show c') pos
  show (ArgumentTypesMismatchE pos t t')    = showE $ expectation "function arguments type mismatch. Expected type" (showV t) (showV t') pos
  show (TypeMismatchE pos t t')             = showE $ expectation "type mismatch. Expected type" (show t) (show t') pos
  show (ConstViolationE pos id t)           = showE ["const violation.", "Trying to assign value to constant", showI id, ", at", showP pos]
  show (ReturnOutOfScopeE pos)              = showE ["return out of scope, at", showP pos]
  show (ReturnTypeMismatchE pos t t')       = showE $ expectation "return type mismatch. Expected return type" (show t) (show t') pos
  show (NoReturnStatementE pos)             = showE ["no return statement at the end of function body, at", showP pos]
  show (ArgumentRedeclarationE pos)         = showE ["argument redeclaration, at", showP pos]
  show (RedeclarationE pos id)              = showE ["redeclaration. Name", showI id, "already in use in this context, at", showP pos]
  show (VoidAssignmentE pos)                = showE ["assignment of value of type", show ITVoid, "at", showP pos]
  show (OpTypesUnequalE pos t t')           = showE ["binary operation type mismatch.", "Left side is of type", show t, "while right side is of type", show t', ", at", showP pos]