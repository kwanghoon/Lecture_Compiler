module Type(Type(..),gentyp) where 

import EitherState

data Type = 
    Unit
  | Bool 
  | Int
  | Float
  | Fun [Type] Type 
  | Tuple [Type]
  | Array Type
  | Var Integer -- type variable
  deriving (Eq, Show)

noType :: Type
noType = Var undefined -- dummy type variable 

gentyp :: Integer -> (Type.Type, Integer)
gentyp c = (Type.Var c, c+1)