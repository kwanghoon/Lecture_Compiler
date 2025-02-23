module Main (main) where

import MainUtil(lexer)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    dispatch args

dispatch :: [String] -> IO ()
dispatch ("lex":fileNames) = mapM_ (prNameToRun lexer) fileNames
dispatch _ = putStrLn "Usage: [lex file1 file2 ...]"

prNameToRun :: (String -> IO b) -> String -> IO b
prNameToRun cmd fileName =
  do putStrLn fileName 
     cmd fileName
