module Instr(Operand(..), Instr(..)) where 

import Expr (OpKind)

data Operand = 
    VarOp String 
  | LitOp Int
  deriving (Show,Eq)
  
data Instr = 
    Push Operand
  | Pop 
  | Store String 
  | InstrOp OpKind 
  | IfZ [Instr] [Instr]
  deriving (Show,Eq)