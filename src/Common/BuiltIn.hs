module Common.BuiltIn
  ( isBuiltIn
  , evalBuiltIn
  , builtInFuncTypes
  ) where
import           Control.Monad.Except (MonadError (throwError),
                                       MonadIO (liftIO))
import           Evaluator.Exceptions (RuntimeException (UnkownE))
import           Evaluator.Memory     (Value (ValBool, ValEmpty, ValInt, ValStr))
import           Evaluator.Monads     (EvalM)
import           Generated.Syntax     (Ident (..))
import           Typechecker.Types    (InternalType (ITBool, ITFun, ITInt, ITStr, ITVoid),
                                       Mutability (Imm), ValueType)

printStr, showInt, showBoolean :: String
printStr = "Println"
showInt = "ShowInt"
showBoolean = "ShowBoolean"

builtInFuncTypes :: [(Ident, ValueType)]
builtInFuncTypes = [
  (Ident printStr, (ITFun [(ITStr, Imm)] ITVoid, Imm)),
  (Ident showInt, (ITFun [(ITInt, Imm)] ITStr, Imm)),
  (Ident showBoolean, (ITFun [(ITBool, Imm)] ITStr, Imm))
  ]

builtInFuncNames :: [String]
builtInFuncNames = map ((\ (Ident x) -> x) . fst) builtInFuncTypes

isBuiltIn :: Ident -> Bool
isBuiltIn (Ident name) = name `elem` builtInFuncNames

evalBuiltIn :: Ident -> [Value] -> EvalM
evalBuiltIn (Ident name) [value]
  | name == printStr    = liftIO $ putStrLn (showValue value) >> return ValEmpty
  | name == showInt     = return . ValStr $ showValue value
  | name == showBoolean = return . ValStr $ showValue value

evalBuiltIn _ _ = throwError $ UnkownE Nothing

showValue :: Value -> String
showValue (ValStr s)  = s
showValue (ValInt n)  = show n
showValue (ValBool b) = show b
showValue _           = ""
