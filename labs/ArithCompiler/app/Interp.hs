module Interp(evalExpr, evalSeq) where

import Expr ( OpKind(OPDIV, OPADD, OPSUB, OPMUL), Expr(..) ) 

import qualified Data.Map as Map

type Env = Map.Map String Int

evalExpr :: Expr -> Env -> (Int, Env)
evalExpr (Var x) env = 
    case Map.lookup x env of
        Just v -> (v, env)
        Nothing -> error $ "Variable " ++ x ++ " not found"

evalExpr (Assign x e) env =
    let (v, env') = evalExpr e env
    in (v, Map.insert x v env')

evalExpr (Lit n) env = (n, env)

evalExpr (BinOp op e1 e2) env =
    let (v1, env1) = evalExpr e1 env
        (v2, env2) = evalExpr e2 env1
    in case op of
        OPADD -> (v1 + v2, env2)
        OPSUB -> (v1 - v2, env2)
        OPMUL -> (v1 * v2, env2)
        OPDIV -> (v1 `div` v2, env2)

evalExpr (IfZero e1 e2 e3) env =
    let (v1, env1) = evalExpr e1 env
    in if v1 == 0 then evalExpr e2 env1 else evalExpr e3 env1
    
evalSeq :: [Expr] -> Env -> Env
evalSeq [] env = env
evalSeq [e] env = 
    let (_, env') = evalExpr e env in env'
evalSeq (e:es) env = 
    let (_, env') = evalExpr e env
    in evalSeq es env'

