module Lexer(lexerSpec) where

import CommonParserUtil
import Token

import qualified Control.Monad.Trans.State.Lazy as ST
import qualified Data.Map as Map

mkFn :: Token -> LexAction Token IO () -- (String -> Maybe Token)
mkFn tok = \text -> return (Just tok)

skip :: LexAction Token IO ()          -- String -> Maybe Token
skip = \text -> return Nothing

lexerSpec :: LexerSpec Token IO ()
lexerSpec = LexerSpec
  {
    endOfToken    = END_OF_TOKEN,
    lexerSpecList = 
      [ ("/\\*", multiLineCommentBegin),
        ("//" ++ zeroOrMore "[^\n]" ++ "[\n]", skip ), -- rewritten // as this
        ("[ \t\n]", skip),
        ("\\=\\="    , mkFn EQUAL),
        ("\\!\\="    , mkFn NOTEQUAL),
        ("\\<\\="    , mkFn LESSTHANOREQUAL),
        ("\\<\\="    , mkFn GREATERHANOREQUAL),
        ("\\&\\&"    , mkFn AND),
        ("\\|\\|"    , mkFn OR),
        ("\\+\\+"    , mkFn INCREMENT),
        ("\\-\\-"    , mkFn DECREMENT),
        ("\\+\\="    , mkFn ADDASSIGN),
        ("\\-\\="    , mkFn SUBASSIGN),
        ("\\*\\="    , mkFn MULASSIGN),
        ("\\/\\="    , mkFn DIVASSIGN),
        ("\\%\\="    , mkFn MODASSIGN),
        
        ("\\("    , mkFn OPENPAREN),
        ("\\)"    , mkFn CLOSEPAREN),
        ("\\,"    , mkFn COMMA),
        ("\\{"    , mkFn OPENBRACE),
        ("\\}"    , mkFn CLOSEBRACE),
        ("\\;"    , mkFn SEMICOLON),
        ("\\="    , mkFn ASSIGN),
        ("\\["    , mkFn OPENBRACKET),
        ("\\]"    , mkFn CLOSEBRACKET),
        ("\\>"    , mkFn GREATER),
        ("\\<"    , mkFn LESS),
        ("\\+"    , mkFn ADD),
        ("\\-"    , mkFn SUB),
        ("\\*"    , mkFn MUL),
        ("\\/"    , mkFn DIV),
        ("\\%"    , mkFn MOD),
        ("\\!"    , mkFn NOT),

        ("[1-9][0-9]*|0([0-7]+|(x|X)[0-9A-Fa-f]*)?" , mkFn NUMBER),
        ("[A-Za-z_][A-Za-z0-9_]*"    , keywordOrIdentifier)
      ]
  } 

keywordOrIdentifier :: Monad m => String -> m (Maybe Token)
keywordOrIdentifier text = 
  case Map.lookup text keywordMap of
    Nothing -> return $ Just IDENTIFIER
    Just tok -> return $ Just tok

keywordMap :: Map.Map String Token
keywordMap = Map.fromList (map swap keywords)
  where swap (a,b) = (b,a)

-- Regex writer utilities
zeroOrMore x = paren x ++ "*"     -- "(" ++ x ++ ")*"

paren x = "(" ++ x ++ ")"

multiLineCommentBegin :: LexAction Token IO ()          -- String -> Maybe Token
multiLineCommentBegin = \text0 -> -- /*
  --trace ("multiLineCommentBegin" ++ text0) $
    do  (state_parm_, line, col, text) <- ST.get
        let (newLine, newCol, newText) = mlc (tail (tail text)) line (col+2)
        -- lift $ putStrLn text0
        -- lift $ putStrLn (show line ++ ", " ++ show col ++ ", " ++ text)
        -- lift $ putStrLn (show newLine ++ ", " ++ show newCol ++ ", " ++ newText)
        ST.put (state_parm_, newLine, newCol, newText)
        return Nothing

  where
    mlc [] line col = (line, col, [])
    mlc ('*':'/':text) line col = (line, col+2, text)
    mlc ('\n':text) line col = mlc text (line+1) col
    mlc ('\r':text) line col = mlc text (line+1) col
    mlc (_:text) line col = mlc text line (col+1)

