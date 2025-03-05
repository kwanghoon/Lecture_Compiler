module VM(runVM, runInstrList, runInstr) where 

import Expr(OpKind(..))
import Instr 

import qualified Data.Map as Map

type Env = Map.Map String Int
type Stack = [Int]

push :: n -> [n] -> [n]
push x stk = x : stk

pop :: [n] -> (n, [n])
pop [] = error ("stack: pop from the empty stack")
pop (n:stk) = (n, stk)

runVM :: [Instr] -> (Env, Stack)
runVM instrList = runInstrList instrList Map.empty []

runInstrList :: [Instr] -> Env -> Stack -> (Env, Stack) 
runInstrList [] env stack = (env, stack)
runInstrList (i:is) env stack = 
  let (env1,stack1) = runInstr i env stack
      (env2,stack2) = runInstrList is env1 stack1 
  in  (env2, stack2)

runInstr :: Instr -> Env -> Stack -> (Env, Stack)
runInstr (Push (VarOp x)) env stack = 
  case Map.lookup x env of 
    Nothing -> error ("env: Not found: " ++ x)
    Just n -> (env, push n stack)

runInstr (Push (LitOp n)) env stack = (env, push n stack)

runInstr (Pop) env stack = 
  let (_, stack1) = pop stack in (env, stack1)

runInstr (Store x) env stack = 
  let (n, stack1) = pop stack in (Map.insert x n env, stack1)

runInstr (InstrOp op) env stack =
  let (n1,stack1) = pop stack
      (n2,stack2) = pop stack1 
  in case op of 
      OPADD -> (env, push (n2 + n1) stack2) 
      OPSUB -> (env, push (n2 - n1) stack2) 
      OPMUL -> (env, push (n2 * n1) stack2) 
      OPDIV -> (env, push (n2 `div` n1) stack)