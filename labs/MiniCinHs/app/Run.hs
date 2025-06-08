module Run where

import CommonParserUtil

import TokenInterface
import Lexer
import Terminal
import Parser
import Expr
import Comp
import UCode

import Control.Monad (when)
import System.IO

doProcess verbose fileName = do
  text <- readFile fileName
  -- when (verbose) $ putStrLn "Lexing..."
  -- terminalList <- lexing lexerSpec text
  when verbose $ putStrLn "Parsing..."
  exprSeqAst <- parsing False
                  parserSpec ((), 1, 1, text)
                    (aLexer lexerSpec) (fromToken (endOfToken lexerSpec))
  
  when verbose $ putStrLn "Pretty Printing..."
  when verbose $ putStrLn (pprintParse exprSeqAst)
  when verbose $ putStrLn "Compiling to U-code..."
  let ucode = compile (translationUnitFrom exprSeqAst)
  when verbose $ mapM_ (putStrLn . pprintUCode) ucode
  return (pprintParse exprSeqAst)

pprintParse :: ParseTree -> String 
pprintParse = show