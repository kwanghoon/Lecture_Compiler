module Main (main) where

import MainUtil(lexer,parser,evaler,compiler,runner)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    dispatch args

dispatch :: [String] -> IO ()
dispatch ("lex":fileNames) = 
  mapM_ (prNameToRun lexer) fileNames
dispatch ("parse":fileNames) = 
  mapM_ (prNameToRun parser) fileNames
dispatch ("eval":fileNames) =
  mapM_ (prNameToRun evaler) fileNames
dispatch ("compile":fileNames) =
  mapM_ (prNameToRun compiler) fileNames 
dispatch ("run":fileNames) =
  mapM_ (prNameToRun runner) fileNames     
dispatch _ = putStrLn "Usage: [lex|parse|eval|compile|run file1 file2 ...]"

prNameToRun :: (String -> IO b) -> String -> IO b
prNameToRun cmd fileName =
  do putStrLn fileName 
     cmd fileName
