module Token(Token(..), keywords, fromToken) where

import TokenInterface

data Token = 
    END_OF_TOKEN
  | OPENPAREN
  | CLOSEPAREN
  | IDENTIFIER
  | INTEGERNUMBER
  | ADD
  | SUB
  | MUL 
  | DIV 
  | EQUAL
  | SEMICOLON
  | IFZERO
  | THEN 
  | ELSE
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (END_OF_TOKEN,  "$")  -- 
  , (OPENPAREN,     "(" )
  , (CLOSEPAREN,    ")" )
  , (IDENTIFIER,    "identifier")
  , (INTEGERNUMBER, "integer_number")
  , (ADD,           "+")
  , (SUB,           "-")
  , (MUL,           "*")
  , (DIV,           "/")
  , (EQUAL,         "=")
  , (SEMICOLON,     ";")
  , (IFZERO,        "ifzero")
  , (THEN,          "then")
  , (ELSE,          "else")
  ]

keywords :: [(Token, String)]
keywords =
  [ 
    (IFZERO, "ifzero"),
    (THEN,   "then"),
    (ELSE,   "else")
  ]  

findTok :: Token -> [(Token, String)] -> Maybe String
findTok _tok [] = Nothing
findTok tok ((tok_,str):list)
  | tok == tok_ = Just str
  | otherwise   = findTok tok list

instance TokenInterface Token where
  fromToken tok =
    case findTok tok tokenStrList of
      Nothing  -> error ("fromToken: " ++ show tok)
      Just str -> str
  
  isEOT END_OF_TOKEN = True
  isEOT _            = False  