module Expr(Expr(..), OpKind(..), ExprList) where 

data Expr = 
    Var String
  | Assign String Expr
  | Lit Int
  | BinOp OpKind Expr Expr 
  | IfZero Expr Expr Expr
  deriving (Show,Eq)

data OpKind = OPADD | OPSUB | OPMUL | OPDIV
  deriving (Show,Eq)

type ExprList =[Expr]