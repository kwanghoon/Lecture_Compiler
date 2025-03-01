module ParserState(ParserState(..), initParserState, getCounter, setCounter) where

data ParserState = ParState
  {
    counter :: Integer  -- ^ Counter for generating unique names
  }

initParserState :: ParserState
initParserState = ParState
  {
    counter = 0
  }

getCounter :: ParserState -> Integer
getCounter = counter

setCounter :: Integer -> ParserState -> ParserState
setCounter n s = s { counter = n }