module MainUtil(lexer, lex, parser, parse, evaler, eval, compiler, compile) where

import Prelude hiding (lex)
import qualified Data.Map as Map

import Expr
import TokenInterface(fromToken)
import Lexer (lexerSpec)
import Parser (parserSpec, progFrom)
import Terminal (terminalToString)
import CommonParserUtil (lexing, parsing, aLexer, endOfToken)
import ParserState (initParserState)
import Interp
import Compiler

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
     let e = progFrom ast       
     putStrLn (show e)

evaler :: String -> IO ()
evaler fileName =
  do text <- readFile fileName
     eval text

eval :: String -> IO ()
eval text =
  do ast <- 
       parsing False 
         parserSpec (initParserState,1,1,text)
         (aLexer lexerSpec)
         (fromToken (endOfToken lexerSpec))
     let es = progFrom ast
     let env = evalSeq es Map.empty
     putStrLn (show (Map.toList env))

compiler :: String -> IO ()
compiler fileName =
  do text <- readFile fileName
     compile text

compile :: String -> IO ()
compile text = 
  do ast <- 
       parsing False 
         parserSpec (initParserState,1,1,text)
         (aLexer lexerSpec)
         (fromToken (endOfToken lexerSpec))
     let es = progFrom ast
     prInstrList (compList es)
  where 
    prInstrList [] = return ()
    prInstrList (i:is) = 
      do putStrLn (show i)
         prInstrList is

