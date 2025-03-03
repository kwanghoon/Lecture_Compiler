module Expr(Expr(..), ExprList) where 

data Expr = Var String
  | Assign String Expr
  deriving (Show,Eq)

type ExprList =[Expr]