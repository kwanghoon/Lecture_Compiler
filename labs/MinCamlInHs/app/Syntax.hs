module Syntax(Exp(..),Fundef(..),Ident) where 

-- Abstract syntax token
import Type

type Ident = String

data Exp
  = Unit
  | Bool Bool
  | Int Int
  | Float Double
  | Not Exp
  | Neg Exp
  | Add Exp Exp
  | Sub Exp Exp
  | FNeg Exp
  | FAdd Exp Exp
  | FSub Exp Exp
  | FMul Exp Exp
  | FDiv Exp Exp
  | Eq Exp Exp
  | LE Exp Exp
  | If Exp Exp Exp
  | Let (Ident, Type) Exp Exp
  | Var Ident
  | LetRec Fundef Exp
  | App Exp [Exp]
  | Tuple [Exp]
  | LetTuple [(Ident, Type)] Exp Exp
  | Array Exp Exp
  | Get Exp Exp
  | Put Exp Exp Exp
  deriving (Show, Eq)

data Fundef = Fundef
  { name :: (Ident, Type)
  , args :: [(Ident, Type)]
  , body :: Exp
  } deriving (Show, Eq)