module Compiler(compList, comp) where 

import Expr 
import Instr 

compList :: [Expr] -> [Instr]
compList [] = []
compList (expr:exprList) = 
  comp expr ++ [Pop] ++ compList exprList

comp :: Expr -> [Instr]
comp (Var x) = [Push (VarOp x)]  
comp (Lit n) = [Push (LitOp n)]
comp (Assign x expr) =
  comp expr ++ [Store x] ++ [Push (VarOp x)]
comp (BinOp opkind expr1 expr2) =
  comp expr1 ++ comp expr2 ++ [InstrOp opkind]