{-# LANGUAGE LambdaCase #-}
module Interpreter (interpret) where
import           Control.Arrow           (ArrowChoice (left))
import           Evaluator.Evaluator     (eval)
import           Generated.Parser        (myLexer, pProgram)
import           System.Exit             (exitFailure, exitSuccess)
import           System.IO               (hPrint, stderr)
import           Typechecker.Typechecker (typecheck)

exit :: IO (Either String a) -> IO ()
exit computation = computation >>= \case
  Left err -> hPrint stderr err >> exitFailure
  Right _  -> exitSuccess

interpret :: String -> IO ()
interpret = exit . eval' . typecheck' . pProgram . myLexer where
  eval' = either (return . Left) (fmap (left show) . eval)
  typecheck' = either Left (left show . typecheck)
