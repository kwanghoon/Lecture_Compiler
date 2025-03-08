module Id(idOfTyp, gentmp) where

import Type
import ParserState(ParserState,getCounter,setCounter)

idOfTyp :: Type.Type -> String
idOfTyp Type.Unit = "u"
idOfTyp Type.Bool = "b"
idOfTyp Type.Int = "i"
idOfTyp Type.Float = "d"
idOfTyp (Type.Fun ts t) = "f" ++ concatMap idOfTyp ts ++ "_" ++ idOfTyp t
idOfTyp (Type.Tuple ts) = "t" ++ concatMap idOfTyp ts
idOfTyp (Type.Array t) = "a" ++ idOfTyp t
idOfTyp (Type.Var _) = error "idOfTyp: Var Unexpected"

gentmp :: Type.Type -> ParserState -> (String, ParserState)
gentmp t s = 
  let c = getCounter s in 
    (idOfTyp t ++ show c, setCounter (c + 1) s)