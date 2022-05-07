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

builtInFuncTypes :: [(Ident, IType)]
builtInFuncTypes = [
  (Ident printStr, ITFun [ITStr] ITVoid),
  (Ident showInt, ITFun [ITInt] ITStr),
  (Ident showBoolean, ITFun [ITBool] ITStr)]

builtInFuncNames :: [String]
builtInFuncNames = map ((\ (Ident x) -> x) . fst) builtInFuncTypes

isBuiltIn :: Ident -> Bool
isBuiltIn (Ident name) = name `elem` builtInFuncNames

evalBuiltIn :: Ident -> [Value] -> EvalM
evalBuiltIn (Ident name) [value]
  | name == printStr    = liftIO $ putStrLn (showValue value) >> return ValEmpty
  | name == showInt     = return . ValString $ showValue value
  | name == showBoolean = return . ValString $ showValue value

evalBuiltIn _ _ = throwError $ UnkownE Nothing

showValue :: Value -> String
showValue (ValString s) = s
showValue (ValInt n)    = show n
showValue (ValBool b)   = show b
showValue _             = ""
