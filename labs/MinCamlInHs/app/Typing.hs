module Typing(tychecker) where

import qualified Type as T
import Syntax
import qualified Data.Map as Map
import EitherState ( _Left, _Right, _fresh, _run, Either_ )

--
_gentyp :: Either_ String T.Type
_gentyp = do
    n <- _fresh
    return (fst (T.gentyp "T" n))

--
tychecker :: Exp -> TyEnv -> Either String (Exp, T.Type, Subst, Subst)
tychecker e extenv = 
  do (t, subst) <- _run (tyChk e extenv Map.empty)
     let subst1 = mkGround subst
     let e1 = applySubstExp subst1 e 
     let t1 = applySubstTyp subst1 t
     let (extSubst, intSubst) = splitExternal subst1
     return (e1, t1, extSubst, intSubst)

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
            Nothing -> let x' = x       -- Todo: Hack!
                           t = T.Var x'
                       in _Right (t, extendSubst x' t subst) 
tyChk e@(LetRec (x, t) _args e1 e2) tyenv subst = do
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
    else _Right (extendSubst r1 t2 subst)
unify' e t1 (T.Var r2) subst =
    if occur r2 t1 then _Left ("occurs check fails:" ++  " in " ++ show e)
    else _Right (extendSubst r2 t1 subst)    
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

extendSubst :: String -> T.Type -> Subst -> Subst
extendSubst x t subst = 
    let substList = Map.toList subst 
        t1 = applySubst substList t
        subst1 = Map.map (applySubst [(x,t1)]) subst
    in Map.insert x t1 subst1

--
mkGround :: Subst -> Subst
mkGround = Map.map mkGndTyp

mkGndTyp :: T.Type -> T.Type
mkGndTyp T.Unit = T.Unit
mkGndTyp T.Bool = T.Bool
mkGndTyp T.Int = T.Int
mkGndTyp T.Float = T.Float
mkGndTyp (T.Fun ts t) = T.Fun (mkGndTyps ts) (mkGndTyp t)
mkGndTyp (T.Tuple ts) = T.Tuple (mkGndTyps ts)
mkGndTyp (T.Array t) = T.Array (mkGndTyp t)
mkGndTyp (T.Var _v) = T.Int -- Instantiate type variables with Int

mkGndTyps :: [T.Type] -> [T.Type]
mkGndTyps = map mkGndTyp

splitExternal :: Subst -> (Subst, Subst)
splitExternal =
    Map.partitionWithKey 
        (\k _v -> head k /= 'P' && head k /= 'T')  -- Todo: Hack!

applySubstTyp :: Subst -> T.Type -> T.Type
applySubstTyp subst = applySubst (Map.toList subst)

applySubstExp :: Subst -> Exp -> Exp
applySubstExp _subst Unit = Unit
applySubstExp _subst (Bool b) = Bool b
applySubstExp _subst (Int i) = Int i
applySubstExp _subst (Float f) = Float f
applySubstExp subst (Not e) = Not (applySubstExp subst e)
applySubstExp subst (Neg e) = Neg (applySubstExp subst e)
applySubstExp subst (Add e1 e2) = 
    Add (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (Sub e1 e2) = 
    Sub (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (FNeg e) = FNeg (applySubstExp subst e)
applySubstExp subst (FAdd e1 e2) = 
    FAdd (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (FSub e1 e2) = 
    FSub (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (FMul e1 e2) = 
    FMul (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (FDiv e1 e2) = 
    FDiv (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (Eq e1 e2) = 
    Eq (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (LE e1 e2) = 
    LE (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (If e1 e2 e3) = 
    If (applySubstExp subst e1) 
        (applySubstExp subst e2) (applySubstExp subst e3)
applySubstExp subst (Let (x, t) e1 e2) = 
    Let (x, applySubstTyp subst t) 
        (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp _subst (Var x) = Var x
applySubstExp subst (LetRec (x, t) _args e1 e2) = 
    LetRec (x, applySubstTyp subst t) 
           (map (\(y, t1) -> (y, applySubstTyp subst t1)) _args) 
           (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (App e es) = 
    App (applySubstExp subst e) (map (applySubstExp subst) es)
applySubstExp subst (Tuple es) = Tuple (map (applySubstExp subst) es)
applySubstExp subst (LetTuple xts e1 e2) = 
    LetTuple (map (\(x, t) -> (x, applySubstTyp subst t)) xts) 
        (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (Array e1 e2) = 
    Array (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (Get e1 e2) = 
    Get (applySubstExp subst e1) (applySubstExp subst e2)
applySubstExp subst (Put e1 e2 e3) = 
    Put (applySubstExp subst e1) 
            (applySubstExp subst e2) 
                (applySubstExp subst e3)

--
foldrM :: (a -> b -> Either_ String b) -> b -> [a] -> Either_ String b
foldrM _ z []     = return z
foldrM f z (x:xs) = do
    rest <- foldrM f z xs
    f x rest

--
