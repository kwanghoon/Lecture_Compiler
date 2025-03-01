module Id where

import qualified Type as T
import ParserState(ParserState,getCounter,setCounter)

idOfTyp :: T.Type -> String
idOfTyp T.UnitType = "u"
idOfTyp T.Bool = "b"
idOfTyp T.Int = "i"
idOfTyp T.Float = "d"
idOfTyp (T.Fun ts t) = "f" ++ concatMap idOfTyp ts ++ "_" ++ idOfTyp t
idOfTyp (T.Tuple ts) = "t" ++ concatMap idOfTyp ts
idOfTyp (T.Array t) = "a" ++ idOfTyp t
idOfTyp (T.Var _) = error "idOfTyp: Var Unexpected"

gentmp :: T.Type -> ParserState -> (String, ParserState)
gentmp t s = 
  let c = getCounter s in 
    (idOfTyp t ++ show c, setCounter (c + 1) s)