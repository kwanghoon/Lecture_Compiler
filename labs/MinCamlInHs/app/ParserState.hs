module ParserState(ParserState(..), initParserState, getCounter, setCounter) where

-- ^ Counter for generating unique names
newtype ParserState = ParState { counter :: Integer }

initParserState :: ParserState
initParserState = ParState { counter = 0 }

getCounter :: ParserState -> Integer
getCounter = counter

setCounter :: Integer -> ParserState -> ParserState
setCounter n s = s { counter = n }