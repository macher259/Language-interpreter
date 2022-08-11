module Main where
import qualified Interpreter as I 
import System.Exit
import qualified ParGrammar as P
import System.IO
import System.Environment
import Control.Monad.Except (runExcept)
import qualified TypeCheck as T
main :: IO () 
main = do
    args <- getArgs
    case args of 
        [path] -> do
                f <- readFile path
                run f
        [] -> do
            contents <- getContents
            run contents
        _ -> die "Bad usage, look at README"


run :: String -> IO ()
run prog = do
    case P.pProgram $ P.myLexer prog of 
      Left s -> hPutStrLn stderr s
      Right pro -> case runExcept $ T.runTypeCheck pro of
        Left err -> hPutStrLn stderr $ "static analysis error: " ++ err
        Right _ -> I.runInterpreter pro
