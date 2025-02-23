module Type(Type(..)) where 

data Type = 
    UnitType
  | Bool 
  | Int
  | Float
  | Fun [Type] Type 
  | Tuple [Type]
  | Array Type
  | Var (Maybe Type)
  deriving (Eq, Show)