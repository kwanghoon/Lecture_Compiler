module Type(Type(..),noType) where 

data Type = 
    Unit
  | Bool 
  | Int
  | Float
  | Fun [Type] Type 
  | Tuple [Type]
  | Array Type
  | Var (Maybe Type)
  deriving (Eq, Show)

noType :: Type
noType = Var Nothing 