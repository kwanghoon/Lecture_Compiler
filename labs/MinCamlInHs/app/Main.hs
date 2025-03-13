module Main (main) where

import MainUtil(lexer, parser, checker, knormalizer, alphaconverter)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    dispatch args

dispatch :: [String] -> IO ()
dispatch ("lex":fileNames) = mapM_ (prNameToRun lexer) fileNames
dispatch ("parse":fileNames) = mapM_ (prNameToRun parser) fileNames
dispatch ("check":fileNames) = mapM_ (prNameToRun checker) fileNames
dispatch ("knorm":fileNames) = mapM_ (prNameToRun knormalizer) fileNames
dispatch ("alpha":fileNames) = mapM_ (prNameToRun alphaconverter) fileNames
dispatch _ = putStrLn "Usage: [lex|parse|check|knorm|alpha] file1 file2 ..."

prNameToRun :: (String -> IO b) -> String -> IO b
prNameToRun cmd fileName =
  do putStrLn fileName 
     cmd fileName
