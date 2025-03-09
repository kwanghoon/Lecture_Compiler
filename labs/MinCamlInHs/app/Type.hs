module Type(Type(..),gentyp) where 

data Type = 
    Unit
  | Bool 
  | Int
  | Float
  | Fun [Type] Type 
  | Tuple [Type]
  | Array Type
  | Var String -- type variable
  deriving (Eq, Show)

gentyp :: String -> Integer -> (Type.Type, Integer)
gentyp prefix c = (Type.Var (prefix ++ show c), c+1)