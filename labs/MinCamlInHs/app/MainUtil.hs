module MainUtil(lexer, parser, lex, parse) where

import Prelude hiding (lex)

import TokenInterface(fromToken)
import Lexer (lexerSpec)
import Parser (parserSpec, expFrom)
import Terminal (terminalToString)
import CommonParserUtil (lexing, parsing, aLexer, endOfToken)
import ParserState (initParserState)

--  Example usage:
--
--  $ stack ghci
--  ghci> lexer ".\\app\\test\\fib.ml"
--  ...

lexer :: String -> IO ()
lexer fileName =
  do text <- readFile fileName
     lex text

lex :: String -> IO ()
lex text = 
  do terminalList <- lexing lexerSpec initParserState text
     mapM_ (putStrLn . terminalToString) terminalList

parser :: String -> IO ()
parser fileName =
  do text <- readFile fileName
     parse text

parse :: String -> IO ()
parse text =
  do ast <- 
       parsing False 
         parserSpec (initParserState,1,1,text)
         (aLexer lexerSpec)
         (fromToken (endOfToken lexerSpec))
     let e = expFrom ast       
     putStrLn (show e)