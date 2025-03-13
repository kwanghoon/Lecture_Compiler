module Alpha(alpha) where 

import Id(gentmp)
import qualified Type as T
import qualified KNormal as K( KNorm(..) )
import Control.Monad.State ( State, get, put, runState )
import qualified Data.Map as Map

type Id = String

_gentmp :: T.Type -> State Integer Id
_gentmp t = do
    c <- get
    let (x, c1) = gentmp t c
    put c1
    return x

find :: Id -> Map.Map Id Id -> Id
find x env = case Map.lookup x env of
    Just y -> y
    Nothing -> x

alpha :: K.KNorm -> K.KNorm
alpha k = k1
    where (k1, _) = runState (aconv Map.empty k) 0

aconv :: Map.Map Id Id -> K.KNorm ->State Integer K.KNorm
aconv _env K.Unit = return K.Unit
aconv _env (K.Int i) = return (K.Int i)
aconv _env (K.Float d) = return (K.Float d)
aconv env (K.Neg x) = return (K.Neg (find x env))
aconv env (K.Add x y) = return (K.Add (find x env) (find y env))
aconv env (K.Sub x y) = return (K.Sub (find x env) (find y env))
aconv env (K.FNeg x) = return (K.FNeg (find x env))
aconv env (K.FAdd x y) = return (K.FAdd (find x env) (find y env))
aconv env (K.FSub x y) = return (K.FSub (find x env) (find y env))
aconv env (K.FMul x y) = return (K.FMul (find x env) (find y env))
aconv env (K.FDiv x y) = return (K.FDiv (find x env) (find y env))
aconv env (K.IfEq x y e1 e2) = do
    e1' <- aconv env e1
    e2' <- aconv env e2
    return (K.IfEq (find x env) (find y env) e1' e2')
aconv env (K.IfLE x y e1 e2) = do
    e1' <- aconv env e1
    e2' <- aconv env e2
    return (K.IfLE (find x env) (find y env) e1' e2')
aconv env (K.Let (x,t) e1 e2) = do
    x' <- _gentmp t
    e1' <- aconv env e1
    e2' <- aconv (Map.insert x x' env) e2
    return (K.Let (x',t) e1' e2')
aconv env (K.Var x) = return (K.Var (find x env))
aconv env (K.LetRec (x,t) yts e1 e2) = do
    x' <- _gentmp t
    let ys = map fst yts
    let ts = map snd yts
    ys' <- mapM (\(_y,t1) -> _gentmp t1) yts
    let env2 = Map.insert x x' env
    let env1 = Map.union (Map.fromList (zip ys ys')) env2
    e1' <- aconv env1 e1
    e2' <- aconv env2 e2
    return (K.LetRec (x',t) (zip ys' ts) e1' e2')
aconv env (K.App x ys) = 
    return (K.App (find x env) (map (\y -> find y env) ys))    
aconv env (K.Tuple xs) = 
    return (K.Tuple (map (\x -> find x env) xs))
aconv env (K.LetTuple xts y e) = do
    let xs = map fst xts
    let ts = map snd xts
    xs' <- mapM _gentmp ts
    let env1 = Map.union (Map.fromList (zip xs xs')) env
    e' <- aconv env1 e
    return (K.LetTuple (zip xs' ts) (find y env) e')
aconv env (K.Get x y) = return (K.Get (find x env) (find y env))
aconv env (K.Put x y z) = 
    return (K.Put (find x env) (find y env) (find z env))
aconv _env (K.ExtArray x) = return (K.ExtArray x)
aconv env (K.ExtFunApp x ys) = 
    return (K.ExtFunApp x (map (\y -> find y env) ys))