module MainUtil(
  lexer, lex, 
  parser, parse, 
  checker, check, 
  knormalizer, knorm) where

import Prelude hiding (lex)

import Lexer (lexerSpec)
import Parser (parserSpec, expFrom)
import Typing (tychecker)
import KNormal (knormal)

import TokenInterface(fromToken)
import Terminal (terminalToString)
import CommonParserUtil (lexing, parsing, aLexer, endOfToken)
import ParserState (initParserState)
import System.Exit (exitFailure)

import qualified Data.Map as Map

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

checker :: String -> IO ()
checker fileName =
  do text <- readFile fileName
     check text 

check :: String -> IO ()
check text =
  do ast <- 
       parsing False 
         parserSpec (initParserState,1,1,text)
         (aLexer lexerSpec)
         (fromToken (endOfToken lexerSpec))
     let e = expFrom ast
     case tychecker e Map.empty of
        Right (typede,ty,extenv,subst) -> putStrLn (show (typede,ty,extenv,subst))
        Left err -> putStrLn err

knormalizer :: String -> IO ()
knormalizer fileName =
  do text <- readFile fileName
     knorm text

knorm :: String -> IO ()
knorm text =
  do ast <- 
       parsing False 
         parserSpec (initParserState,1,1,text)
         (aLexer lexerSpec)
         (fromToken (endOfToken lexerSpec))
     let e = expFrom ast
     (typede,extenv) <-
       case tychecker e Map.empty of
         Right (te,_,extenv,_) -> return (te,extenv)
         Left err -> do putStrLn err; exitFailure
     k <- knormal typede extenv
     putStrLn (show k)
