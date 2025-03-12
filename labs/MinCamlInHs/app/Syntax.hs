module Syntax(Exp(..),Ident) where 

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
  | LetRec (Ident, Type) [(Ident, Type)] Exp Exp
  | App Exp [Exp]
  | Tuple [Exp]
  | LetTuple [(Ident, Type)] Exp Exp
  | Array Exp Exp
  | Get Exp Exp
  | Put Exp Exp Exp
  deriving (Show, Eq)
