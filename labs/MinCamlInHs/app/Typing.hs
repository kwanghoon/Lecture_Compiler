module Typing(tychecker) where

import qualified Type as T
import Syntax
import qualified Data.Map as Map
import EitherState

--
_gentyp :: Either_ String T.Type
_gentyp = do
    n <- _fresh
    return (fst (T.gentyp "T" n))

--
tychecker :: Exp -> TyEnv -> Either String (T.Type, Subst)
tychecker e extenv = 
  _run (tyChk e extenv Map.empty)

type TyEnv = Map.Map String T.Type 
type Subst = Map.Map String T.Type

tyChk :: Exp -> TyEnv -> Subst -> Either_ String (T.Type, Subst)
tyChk Unit _ subst = _Right (T.Unit, subst)
tyChk (Bool _) _ subst = _Right (T.Bool, subst)
tyChk (Int _) _ subst = _Right (T.Int, subst)
tyChk (Float _) _ subst = _Right (T.Float, subst)
tyChk _e@(Not e) tyenv subst = do 
  (t, subst1) <- tyChk e tyenv subst
  subst2 <- unify _e T.Bool t subst1
  _Right (T.Bool, subst2)
tyChk _e@(Neg e) tyenv subst = do
  (t, subst1) <- tyChk e tyenv subst
  subst2 <- unify _e T.Int t subst1
  _Right (T.Int, subst2)
tyChk e@(Add e1 e2) tyenv subst = do
  (t1, subst1) <- tyChk e1 tyenv subst
  (t2, subst2) <- tyChk e2 tyenv subst1
  subst3 <- unify e T.Int t1 subst2
  subst4 <- unify e T.Int t2 subst3
  _Right (T.Int, subst4) 
tyChk e@(Sub e1 e2) tyenv subst = do
  (t1, subst1) <- tyChk e1 tyenv subst
  (t2, subst2) <- tyChk e2 tyenv subst1
  subst3 <- unify e T.Int t1 subst2
  subst4 <- unify e T.Int t2 subst3
  _Right (T.Int, subst4)
tyChk _e@(FNeg e) tyenv subst = do
  (t, subst1) <- tyChk e tyenv subst
  subst2 <- unify e T.Float t subst1
  return (T.Float, subst2)  
tyChk e@(FAdd e1 e2) tyenv subst = do
    (t1, subst1) <- tyChk e1 tyenv subst
    (t2, subst2) <- tyChk e2 tyenv subst1
    subst3 <- unify e T.Float t1 subst2
    subst4 <- unify e T.Float t2 subst3
    _Right (T.Float, subst4)
tyChk e@(FSub e1 e2) tyenv subst = do
    (t1, subst1) <- tyChk e1 tyenv subst
    (t2, subst2) <- tyChk e2 tyenv subst1
    subst3 <- unify e T.Float t1 subst2
    subst4 <- unify e T.Float t2 subst3
    _Right (T.Float, subst4)
tyChk e@(FMul e1 e2) tyenv subst = do
    (t1, subst1) <- tyChk e1 tyenv subst
    (t2, subst2) <- tyChk e2 tyenv subst1
    subst3 <- unify e T.Float t1 subst2
    subst4 <- unify e T.Float t2 subst3
    _Right (T.Float, subst4)    
tyChk e@(FDiv e1 e2) tyenv subst = do
    (t1, subst1) <- tyChk e1 tyenv subst
    (t2, subst2) <- tyChk e2 tyenv subst1
    subst3 <- unify e T.Float t1 subst2
    subst4 <- unify e T.Float t2 subst3
    _Right (T.Float, subst4)
tyChk e@(Eq e1 e2) tyenv subst = do
    (t1, subst1) <- tyChk e1 tyenv subst
    (t2, subst2) <- tyChk e2 tyenv subst1
    subst3 <- unify e t1 t2 subst2
    _Right (T.Bool, subst3)
tyChk e@(LE e1 e2) tyenv subst = do -- Todo: t1 and t2 must be Int?
    (t1, subst1) <- tyChk e1 tyenv subst
    (t2, subst2) <- tyChk e2 tyenv subst1
    subst3 <- unify e t1 t2 subst2
    _Right (T.Bool, subst3)
tyChk e@(If e1 e2 e3) tyenv subst = do
    (t1, subst1) <- tyChk e1 tyenv subst
    (t2, subst2) <- tyChk e2 tyenv subst1
    (t3, subst3) <- tyChk e3 tyenv subst2
    subst4 <- unify e T.Bool t1 subst3
    subst5 <- unify e t2 t3 subst4
    _Right (t2, subst5)
tyChk e@(Let (x, t) e1 e2) tyenv subst = do
    (t1, subst1) <- tyChk e1 tyenv subst
    subst2 <- unify e t t1 subst1
    let tyenv1 = Map.insert x t tyenv
    tyChk e2 tyenv1 subst2    
tyChk _e@(Var x) tyenv subst = 
    case Map.lookup x tyenv of
        Just t -> _Right (t, subst)
        Nothing -> do -- _Left ("unbound variable: " ++ x)
          case Map.lookup x subst of
            Just t -> _Right (t, subst)
            Nothing -> let t = T.Var x in _Right (t, Map.insert x t subst) -- Todo: Hack!
tyChk e@(LetRec (Fundef {name=(x, t), args=_args, body=e1}) e2) tyenv subst = do
    let tyenv1 = Map.insert x t tyenv
    let tyenv2 = foldr (\(y, _t) env -> Map.insert y _t env) tyenv1 _args
    (t1, subst1) <- tyChk e1 tyenv2 subst
    let ft = T.Fun (map snd _args) t1
    subst2 <- unify e t ft subst1
    tyChk e2 tyenv1 subst2
tyChk _e@(App e es) tyenv subst = do
    t <- _gentyp
    (ft1, subst1) <- tyChk e tyenv subst
    (ts, subst2) <- tyChkES (\e1 (ts, _subst) -> do
        (t1, _subst1) <- tyChk e1 tyenv _subst
        _Right (t1:ts, _subst1)) ([], subst1) es
    let ft2 = T.Fun ts t
    subst3 <- unify e ft1 ft2 subst2
    _Right (t, subst3)
tyChk (Tuple es) tyenv subst = do
    (ts, subst2) <- tyChkES (\e (ts, _subst) -> do
        (t, subst1) <- tyChk e tyenv _subst
        _Right (t:ts, subst1)) ([], subst) es
    _Right (T.Tuple ts, subst2)
tyChk e@(LetTuple xts e1 e2) tyenv subst = do
    (t1, subst1) <- tyChk e1 tyenv subst
    let (_, ts) = unzip xts
    subst2 <- unify e (T.Tuple ts) t1 subst1
    let tyenv1 = foldr (\(x, t) env -> Map.insert x t env) tyenv xts
    tyChk e2 tyenv1 subst2
tyChk e@(Array e1 e2) tyenv subst = do
    (t1, subst1) <- tyChk e1 tyenv subst
    (t2, subst2) <- tyChk e2 tyenv subst1
    subst3 <- unify e T.Int t1 subst2
    _Right (T.Array t2, subst3)
tyChk e@(Get e1 e2) tyenv subst = do
    (t1, subst1) <- tyChk e1 tyenv subst
    (t2, subst2) <- tyChk e2 tyenv subst1
    t <- _gentyp
    subst3 <- unify e (T.Array t) t1 subst2
    subst4 <- unify e T.Int t2 subst3
    _Right (t, subst4)
tyChk e@(Put e1 e2 e3) tyenv subst = do
    (t1, subst1) <- tyChk e1 tyenv subst
    (t2, subst2) <- tyChk e2 tyenv subst1
    (t3, subst3) <- tyChk e3 tyenv subst2
    subst4 <- unify e (T.Array t3) t1 subst3
    subst5 <- unify e T.Int t2 subst4
    _Right (T.Unit, subst5)

tyChkES :: (Exp -> ([T.Type], Subst) -> Either_ String ([T.Type], Subst)) 
   -> ([T.Type], Subst) -> [Exp] 
   ->  Either_ String ([T.Type], Subst)
tyChkES = foldrM

unify :: Exp -> T.Type -> T.Type -> Subst -> Either_ String Subst
unify e t1 t2 subst = 
  let t1' = applySubst (Map.toList subst) t1
      t2' = applySubst (Map.toList subst) t2
  in if t1' == t2' then _Right subst 
     else unify' e t1' t2' subst

unify' :: Exp -> T.Type -> T.Type -> Subst -> Either_ String Subst
unify' e (T.Fun ts1 t1) (T.Fun ts2 t2) subst = do
    subst1 <- unify e t1 t2 subst
    unifyList e ts1 ts2 subst1
unify' e (T.Tuple ts1) (T.Tuple ts2) subst = unifyList e ts1 ts2 subst
unify' e (T.Array t1) (T.Array t2) subst = unify e t1 t2 subst
unify' e (T.Var r1) t2 subst = 
    if occur r1 t2 then _Left ("occurs check fails:" ++  " in " ++ show e)
    else _Right (Map.insert r1 t2 subst)
unify' e t1 (T.Var r2) subst =
    if occur r2 t1 then _Left ("occurs check fails:" ++  " in " ++ show e)
    else _Right (Map.insert r2 t1 subst)    
unify' e _t1 _t2 _subst = 
    _Left ("unify: type mismatch: " ++ 
             show _t1 ++ " and " ++ show _t2 ++ " in " ++ show e)

unifyList :: Exp -> [T.Type] -> [T.Type] -> Subst -> Either_ String Subst
unifyList e ts1 ts2 subst = 
    foldrM (\(t1, t2) _subst -> unify e t1 t2 _subst) subst (zip ts1 ts2)

occur :: String -> T.Type -> Bool
occur _ T.Unit = False 
occur _ T.Bool = False
occur _ T.Int = False
occur _ T.Float = False
occur r (T.Fun ts t) = any (occur r) ts || occur r t
occur r (T.Tuple ts) = any (occur r) ts
occur r (T.Array t) = occur r t
occur r (T.Var r') = r == r'

-- 
applySubst :: [(String,T.Type)] -> T.Type -> T.Type
applySubst [] t = t
applySubst _ T.Unit = T.Unit 
applySubst _ T.Bool = T.Bool
applySubst _ T.Int = T.Int
applySubst _ T.Float = T.Float
applySubst ((x, t1):subst) (T.Fun ts t) = 
    applySubst subst 
      (T.Fun (map (applySubst [(x,t1)]) ts) 
             (applySubst [(x,t1)] t))
applySubst ((x, t1):subst) (T.Tuple ts) =
    applySubst subst (T.Tuple (map (applySubst [(x,t1)]) ts))
applySubst ((x, t1):subst) (T.Array t) =
    applySubst subst (T.Array (applySubst [(x,t1)] t))
applySubst ((x, t1):subst) (T.Var y) =
    if x == y then applySubst subst t1 else applySubst subst (T.Var y)

--
foldrM :: (a -> b -> Either_ String b) -> b -> [a] -> Either_ String b
foldrM _ z []     = return z
foldrM f z (x:xs) = do
    rest <- foldrM f z xs
    f x rest