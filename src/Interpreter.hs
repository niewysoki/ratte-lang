module Interpreter (interpretFile, interpret) where
import Control.Monad
import System.IO
import System.Exit
import Evaluator.Evaluator
import Typechecker.Typechecker
import Control.Arrow
import Generated.Parser

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