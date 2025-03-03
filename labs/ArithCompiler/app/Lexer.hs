module Lexer(lexerSpec) where

import CommonParserUtil
import Token 
import ParserState (ParserState)

import qualified Data.Map as Map

mkFn :: Token -> LexAction Token IO ParserState
mkFn tok = \_ -> return $ Just tok

skip :: LexAction Token IO ParserState
skip = \_ -> return $ Nothing

lexerSpec :: LexerSpec Token IO ParserState
lexerSpec = LexerSpec
  {
    endOfToken    = END_OF_TOKEN,
    lexerSpecList = 
      [ 
        ("[ \t\n\r]" , skip)    -- space+ where space = [' ' '\t' '\n' '\r']

        -- Typical definition of keywords or identifiers
        , ("[_a-zA-Z][_a-zA-Z0-9]*", keywordOrIdentifier)
      ]
  }

keywordMap :: Map.Map String Token
keywordMap = Map.fromList (map swap keywords)
  where swap (a,b) = (b,a)

keywordOrIdentifier :: Monad m => String -> m (Maybe Token)
keywordOrIdentifier text = 
  case Map.lookup text keywordMap of
    Nothing -> return $ Just IDENTIFIER
    Just tok -> return $ Just tok