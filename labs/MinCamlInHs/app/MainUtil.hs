module MainUtil(
  lexer, lex, 
  parser, parse, 
  checker, check, 
  knormalizer, knorm,
  alphaconverter, alphaconvert,
  closureconverter, closureconvert) where

import Prelude hiding (lex)

import Lexer (lexerSpec)
import Parser (parserSpec, expFrom)
import Typing (tychecker,initextenv)
import KNormal (knormal)
import Alpha (alpha)
import Closure (cloconv)

import TokenInterface(fromToken)
import Terminal (terminalToString)
import CommonParserUtil (lexing, parsing, aLexer, endOfToken)
import ParserState (initParserState)
import System.Exit (exitFailure)

import qualified Data.Map as Map(union)

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
     case tychecker e initextenv of
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
       case tychecker e initextenv of
         Right (te,_,extenv,_) -> return (te,extenv)
         Left err -> do putStrLn err; exitFailure
     let extenv1 = Map.union extenv initextenv
     k <- knormal typede extenv1
     putStrLn (show k)

alphaconverter :: String -> IO ()
alphaconverter fileName =
  do text <- readFile fileName
     alphaconvert text     

alphaconvert :: String -> IO ()
alphaconvert text =
  do ast <- 
       parsing False 
         parserSpec (initParserState,1,1,text)
         (aLexer lexerSpec)
         (fromToken (endOfToken lexerSpec))
     let e = expFrom ast
     (typede,extenv) <-
       case tychecker e initextenv of
         Right (te,_,extenv,_) -> return (te,extenv)
         Left err -> do putStrLn err; exitFailure
     let extenv1 = Map.union extenv initextenv
     k <- knormal typede extenv1
     let k1 = alpha k
     putStrLn (show k1)

closureconverter :: String -> IO ()
closureconverter fileName =
  do text <- readFile fileName
     closureconvert text

closureconvert :: String -> IO ()
closureconvert text =
  do ast <- 
       parsing False 
         parserSpec (initParserState,1,1,text)
         (aLexer lexerSpec)
         (fromToken (endOfToken lexerSpec))
     let e = expFrom ast
     (typede,extenv) <-
       case tychecker e initextenv of
         Right (te,_,extenv,_) -> return (te,extenv)
         Left err -> do putStrLn err; exitFailure
     let extenv1 = Map.union extenv initextenv
     k <- knormal typede extenv1
     let k1 = alpha k
     p <- cloconv k1
     putStrLn (show p)     