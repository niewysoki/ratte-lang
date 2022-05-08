module Interpreter (interpretFile, interpret) where
import           Control.Arrow           (ArrowChoice (left))
import           Control.Monad           ((<=<))
import           Evaluator.Evaluator     (eval)
import           Generated.Parser        (myLexer, pProgram)
import           System.Exit             (exitFailure, exitSuccess)
import           System.IO               (hPrint, stderr)
import           Typechecker.Typechecker (typecheck)

interpretFile :: FilePath -> IO ()
interpretFile = interpret <=< readFile

exit :: IO (Either String a) -> IO ()
exit comp = do
  result <- comp
  case result of
    Left err -> hPrint stderr err >> exitFailure
    Right _  -> exitSuccess

interpret :: String -> IO ()
interpret = exit . eval' . typecheck' . pProgram . myLexer where
  eval' = either (return . Left) (fmap (left show) . eval)
  typecheck' = either Left (left show . typecheck)
