module Token(Token(..), keywords) where

import TokenInterface

data Token =
    END_OF_TOKEN
  | CONSTR
  | ELSE 
  | IF 
  | INT 
  | RETURN 
  | VOID 
  | WHILE 
  | EQUAL 
  | NOTEQUAL 
  | LESSTHANOREQUAL 
  | GREATERHANOREQUAL 
  | AND 
  | OR 
  | INCREMENT
  | DECREMENT
  | ADDASSIGN 
  | SUBASSIGN 
  | MULASSIGN 
  | DIVASSIGN 
  | MODASSIGN 
  | IDENTIFIER 
  | NUMBER 

  | OPENPAREN
  | CLOSEPAREN
  | COMMA
  | OPENBRACE
  | CLOSEBRACE
  | SEMICOLON
  | ASSIGN
  | OPENBRACKET
  | CLOSEBRACKET
  | GREATER
  | LESS
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | NOT
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (END_OF_TOKEN, "$")
  , (EQUAL, "==")
  , (NOTEQUAL, "!=")
  , (LESSTHANOREQUAL, "<=")
  , (GREATERHANOREQUAL, ">=")
  , (AND, "&&")
  , (OR, "||")
  , (INCREMENT, "++")
  , (DECREMENT, "--")
  , (ADDASSIGN, "+=")
  , (SUBASSIGN, "-=")
  , (MULASSIGN, "*=")
  , (DIVASSIGN, "/=")
  , (MODASSIGN, "%=")
  , (IDENTIFIER, "IDENTIFIER")
  , (NUMBER, "NUMBER")
  , (OPENPAREN, "(")
  , (CLOSEPAREN, ")")
  , (COMMA, ",")
  , (OPENBRACE, "{")
  , (CLOSEBRACE, "}")
  , (SEMICOLON, ";")
  , (ASSIGN, "=")
  , (OPENBRACKET, "[")
  , (CLOSEBRACKET, "]")
  , (GREATER, ">")
  , (LESS, "<")
  , (ADD, "+")
  , (SUB, "-")
  , (MUL, "*")
  , (DIV, "/")
  , (MOD, "%")
  , (NOT, "!")
  ] ++ keywords 

keywords :: [(Token,String)]
keywords =
  [ (IF, "if")
  , (INT, "int")
  , (RETURN, "return")
  , (VOID, "void")
  , (WHILE, "while")
  , (CONSTR, "const")
  , (ELSE, "else")
  ]

findTok tok [] = Nothing
findTok tok ((tok_,str):list)
  | tok == tok_ = Just str
  | otherwise   = findTok tok list

findStr str [] = Nothing
findStr str ((tok,str_):list)
  | str == str_ = Just tok
  | otherwise   = findStr str list

instance TokenInterface Token where
  -- toToken str   =
  --   case findStr str tokenStrList of
  --     Nothing  -> error ("toToken: " ++ str)
  --     Just tok -> tok
  fromToken tok =
    case findTok tok tokenStrList of
      Nothing  -> error ("fromToken: " ++ show tok)
      Just str -> str
  
  isEOT END_OF_TOKEN = True
  isEOT _            = False
  
