module Id(idOfTyp, gentmp, TlvId(..)) where

import Type

newtype TlvId = TlvId String
    deriving (Show)

idOfTyp :: Type.Type -> String
idOfTyp Type.Unit = "u"
idOfTyp Type.Bool = "b"
idOfTyp Type.Int = "i"
idOfTyp Type.Float = "d"
idOfTyp (Type.Fun ts t) = "f" ++ concatMap idOfTyp ts ++ "_" ++ idOfTyp t
idOfTyp (Type.Tuple ts) = "t" ++ concatMap idOfTyp ts
idOfTyp (Type.Array t) = "a" ++ idOfTyp t
idOfTyp (Type.Var _) = error "idOfTyp: Var Unexpected"

gentmp :: Type.Type -> Integer -> (String, Integer)
gentmp t c = (idOfTyp t ++ show c, c + 1)