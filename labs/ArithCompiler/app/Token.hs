module Token(Token(..)) where

import TokenInterface

data Token = END_OF_TOKEN
  {- List token names here -}
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (END_OF_TOKEN, "$")  -- 
    {- List pairs of a token name 
       and a textual representation here -}  
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