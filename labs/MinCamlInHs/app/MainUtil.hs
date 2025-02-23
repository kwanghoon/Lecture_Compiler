module MainUtil(lexer, parser) where

import TokenInterface(fromToken)
import Lexer (lexerSpec)
import Parser (parserSpec, expFrom)
import Terminal (terminalToString)
import CommonParserUtil (lexing, parsing, aLexer, endOfToken)

--  Example usage:
--
--  $ stack ghci
--  ghci> lexer ".\\app\\test\\fib.ml"
--  ...

lexer :: String -> IO ()
lexer fileName =
  do let stateParm = ()
     text <- readFile fileName
     terminalList <- lexing lexerSpec stateParm text
     mapM_ (putStrLn . terminalToString) terminalList

parser :: String -> IO ()
parser fileName =
  do text <- readFile fileName
     ast <- 
       parsing False 
         parserSpec ((),1,1,text)
         (aLexer lexerSpec)
         (fromToken (endOfToken lexerSpec))
     let exp = expFrom ast       
     return ()