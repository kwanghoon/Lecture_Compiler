module Lexer(lexerSpec) where

import CommonParserUtil
import Token

import qualified Data.Map as Map
import qualified Control.Monad.Trans.State.Lazy as ST
import Control.Monad.Trans.Class(lift)

mkFn :: Token -> LexAction Token IO ()
mkFn tok = \_ -> return $ Just tok

skip :: LexAction Token IO ()
skip = \_ -> return $ Nothing

lexerSpec :: LexerSpec Token IO ()
lexerSpec = LexerSpec
  {
    endOfToken    = EOF,
    lexerSpecList = 
      [ 
        ("[ \t\n\r]" , skip),   -- space+ where space = [' ' '\t' '\n' '\r']
        ("\\(\\*", comment),
        ("\\("     , mkFn LPAREN),
        ("\\)"     , mkFn RPAREN),

        ("[0-9]+(\\.[0-9]*)?([eE][+-]?[0-9]+)?", 
                     mkFn CONSTFLOAT),
        ("[0-9]+"  , mkFn CONSTINT),
        
        ("\\-\\."     , mkFn MINUSDOT),
        ("\\+\\."     , mkFn PLUSDOT),
        ("\\-"     , mkFn MINUS),
        ("\\+"     , mkFn PLUS),
        ("\\*\\."     , mkFn ASTDOT),
        ("\\/\\."     , mkFn SLASHDOT),

        ("\\="     , mkFn EQUAL),
        ("<>"     , mkFn LESSGREATER),
        ("<\\="     , mkFn LESSEQUAL),
        (">\\="     , mkFn GREATEREQUAL),
        ("<"     , mkFn LESS),
        (">"     , mkFn GREATER),

        ("\\,"     , mkFn COMMA),
        
        ("\\." , mkFn DOT),
        ("<-" , mkFn LESSMINUS),

        (";"    , mkFn SEMICOLON),

        ("[_a-zA-Z][_a-zA-Z0-9]*(\\.[_a-zA-Z]+)?", 
         keywordOrIdentifier)
      ]
  }

keywordMap :: Map.Map String Token
keywordMap = Map.fromList (map swap keywords)
  where swap (a,b) = (b,a)

keywordOrIdentifier :: Monad m => String -> m (Maybe Token)
keywordOrIdentifier text = 
  case Map.lookup text keywordMap of
    Nothing -> return $ Just IDENT
    Just tok -> return $ Just tok

-- Invariant: text = "(*..."  
comment :: LexAction Token IO ()
comment _ =
  do (state_parm_, line, col, text) <- ST.get
     (newLine, newCol, newText) <- mlc 1 (tail (tail text)) line (col+2)
     ST.put (state_parm_, newLine, newCol, newText)
     return Nothing
  where
    mlc :: Integer -> String -> Line -> Column -> 
            ST.StateT (LexerParserState ()) IO (Line, Column, String)
    mlc _ [] line col =
      do lift $ putStrLn $ "Lex warning: unclosed comment: " ++ show (line, col)
         return (line, col, [])
    mlc 1 ('*':')':text) line col = do {- lift $ putStrLn (show (line, col+2)); -} return (line, col+2, text)
    mlc n ('*':')':text) line col = do {- lift $ putStr "*)"; -} mlc (n-1) text line (col+2)
    mlc n ('(':'*':text) line col = do {- lift $ putStr "(*"; -} mlc (n+1) text line (col+2) 
    mlc n ('\n':text) line _      = do {- lift $ putStrLn ""; -} mlc n text (line+1) 1
    mlc n ('\r':text) line _      = do {- lift $ putStrLn ""; -} mlc n text (line+1) 1
    mlc n (_:text) line col       = do {- lift $ putStr (c:[]); -} mlc n text line (col+1)
     
