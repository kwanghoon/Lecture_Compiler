module Main (main) where

import MainUtil(lexer, parser, checker)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    dispatch args

dispatch :: [String] -> IO ()
dispatch ("lex":fileNames) = mapM_ (prNameToRun lexer) fileNames
dispatch ("parse":fileNames) = mapM_ (prNameToRun parser) fileNames
dispatch ("check":fileNames) = mapM_ (prNameToRun checker) fileNames
dispatch _ = putStrLn "Usage: [lex|parse|check] file1 file2 ..."

prNameToRun :: (String -> IO b) -> String -> IO b
prNameToRun cmd fileName =
  do putStrLn fileName 
     cmd fileName
