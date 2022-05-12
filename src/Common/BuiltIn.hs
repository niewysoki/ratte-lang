module Common.BuiltIn
  ( isBuiltIn
  , evalBuiltIn
  , builtInFuncTypes
  ) where
import           Control.Monad.Except
import           Evaluator.Exceptions
import           Evaluator.Memory
import           Evaluator.Monads
import           Generated.Syntax
import           Typechecker.Types

printStr, showInt, showBoolean :: String
printStr = "Println"
showInt = "ShowInt"
showBoolean = "ShowBoolean"

builtInFuncTypes :: [(Ident, ValueType)]
builtInFuncTypes = [
  (Ident printStr, (ITFunction [(ITStr, Imm)] ITVoid, Imm)),
  (Ident showInt, (ITFunction [(ITInt, Imm)] ITStr, Imm)),
  (Ident showBoolean, (ITFunction [(ITBool, Imm)] ITStr, Imm))
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
