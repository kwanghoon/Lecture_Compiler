module Token(Token(..), keywords) where

import TokenInterface

data Token =
    EOF
    
  | LPAREN
  | RPAREN
  | CONSTTRUE
  | CONSTFALSE
  | NOT
  | CONSTINT
  | CONSTFLOAT
  | MINUS
  | PLUS
  | MINUSDOT
  | PLUSDOT
  | ASTDOT
  | SLASHDOT
  | EQUAL
  | LESSGREATER  -- <> not eqaual!
  | LESSEQUAL    -- >=
  | GREATEREQUAL -- >=
  | LESS          -- <
  | GREATER       -- >
  | IF
  | THEN 
  | ELSE 
  | LET
  | IN 
  | REC 
  | COMMA
  | IDENT         -- _ 
  | ARRAYCREATE
  | DOT 
  | LESSMINUS     -- <- 
  | SEMICOLON
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (EOF,  "$"),
    (LPAREN,       "("),
    (RPAREN,       ")"),
    (CONSTINT,     "const_int"),
    (CONSTFLOAT,   "const_float"),
    (MINUS,        "-"),
    (PLUS,         "+"),
    (MINUSDOT,     "-."),
    (PLUSDOT,      "+."),
    (ASTDOT,       "*."),
    (SLASHDOT,     "/."),
    (EQUAL,         "="),
    (LESSGREATER,  "<>"),
    (LESSEQUAL,    "<="),
    (GREATEREQUAL, ">="),
    (LESS,         "<"),
    (GREATER,      ">"),
    (COMMA,        ","),
    (DOT,          "."),
    (LESSMINUS,    "<-"),
    (SEMICOLON,    ";")
  ] ++ keywords

keywords :: [(Token, String)]
keywords =
  [
    (CONSTTRUE,  "true"), 
    (CONSTFALSE, "false"),
    (NOT,         "not"),
    (IF,          "if"), 
    (THEN,        "then"), 
    (ELSE,        "else"), 
    (LET,         "let"),
    (IN,          "in"), 
    (REC,         "rec"),
    (ARRAYCREATE, "Array.create")
  ]

findTok :: Token -> [(Token, String)] -> Maybe String
findTok tok [] = Nothing
findTok tok ((tok_,str):list)
  | tok == tok_ = Just str
  | otherwise   = findTok tok list

findStr :: Token -> [(String, Token)] -> Maybe String
findStr str [] = Nothing
findStr str ((tok,str_):list)
  | str == str_ = Just tok
  | otherwise   = findStr str list

instance TokenInterface Token where
  fromToken tok =
    case findTok tok tokenStrList of
      Nothing  -> error ("fromToken: " ++ show tok)
      Just str -> str
  

  isEOT EOF = True
  isEOT _   = False  