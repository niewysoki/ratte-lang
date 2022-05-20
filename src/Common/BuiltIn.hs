module Common.BuiltIn
  ( isBuiltIn
  , evalBuiltIn
  , builtInFuncTypes
  ) where
import           Common.Utils
import           Control.Monad.Except (MonadError (throwError),
                                       MonadIO (liftIO))
import           Evaluator.Exceptions
import           Evaluator.Memory
import           Evaluator.Monads
import           Generated.Syntax
import           Typechecker.Types

printStr, showInt, showBoolean, concatFun :: String
printStr = "Println"
showInt = "ShowInt"
showBoolean = "ShowBoolean"
concatFun = "Concat"

builtInFuncTypes :: [(Ident, ValueType)]
builtInFuncTypes = [
  (Ident printStr, (ITFun [(ITStr, Imm)] ITVoid, Imm)),
  (Ident showInt, (ITFun [(ITInt, Imm)] ITStr, Imm)),
  (Ident showBoolean, (ITFun [(ITBool, Imm)] ITStr, Imm)),
  (Ident concatFun, (ITFun [(ITStr, Imm), (ITStr, Imm)] ITStr, Imm))
  ]

builtInFuncNames :: [String]
builtInFuncNames = map (showI . fst) builtInFuncTypes

isBuiltIn :: Ident -> Bool
isBuiltIn (Ident name) = name `elem` builtInFuncNames

evalBuiltIn :: Ident -> [Value] -> EvalValueM
evalBuiltIn (Ident name) [value]
  | name == printStr    = liftIO $ putStrLn (showValue value) >> return ValEmpty
  | name == showInt     = return . ValStr $ showValue value
  | name == showBoolean = return . ValStr $ showValue value

evalBuiltIn (Ident name) vals
  | name == concatFun   = return . ValStr $ concatMap showValue vals

evalBuiltIn _ _ = throwError $ UnknownE Nothing

showValue :: Value -> String
showValue (ValStr s)  = s
showValue (ValInt n)  = show n
showValue (ValBool b) = show b
showValue _           = ""
