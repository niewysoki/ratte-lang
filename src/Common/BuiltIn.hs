module Common.BuiltIn(isBuiltIn, evalBuiltIn) where
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

builtInFuncTypes :: [(Ident, InternalType)]
builtInFuncTypes = [
  (Ident printStr, ITFunction [(ITStr, Imm)] ITVoid),
  (Ident showInt, ITFunction [(ITInt, Imm)] ITStr),
  (Ident showBoolean, ITFunction [(ITBool, Imm)] ITStr)]

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
showValue (ValStr s) = s
showValue (ValInt n)    = show n
showValue (ValBool b)   = show b
showValue _             = ""
