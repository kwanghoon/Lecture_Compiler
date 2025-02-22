module MainUtil(lexer) where

import Lexer (lexerSpec)
import Terminal (terminalToString)
import CommonParserUtil (lexing)

{-
 $ stack ghci
 ghci> lexer ".\\app\\test\fib.ml"
 ...
 an output of the lexer
-}

lexer :: String -> IO ()
lexer fileName =
  do let stateParm = ()
     text <- readFile fileName
     terminalList <- lexing lexerSpec stateParm text
     mapM_ (putStrLn . terminalToString) terminalList