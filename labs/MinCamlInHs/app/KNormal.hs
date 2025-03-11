module KNormal(KNorm(..),KFunDef(..)) where

import Id(gentmp)
import qualified Type as T
import qualified Syntax as E
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State ( StateT, get, put )

type Id = String

data KNorm = 
      Unit
    -- | Bool Bool
    | Int Int
    | Float Double
    -- | Not Id
    | Neg Id
    | Add Id Id
    | Sub Id Id
    | FNeg Id
    | FAdd Id Id
    | FSub Id Id
    | FMul Id Id
    | FDiv Id Id
    | IfEq Id Id KNorm KNorm
    | IfLE Id Id KNorm KNorm
    | Let (Id, T.Type) KNorm KNorm
    | Var Id
    | LetRec KFunDef KNorm
    | App Id [Id]
    | Tuple [Id]
    | LetTuple [(Id, T.Type)] Id KNorm
    | Get Id Id
    | Put Id Id Id
    | ExtArray Id
    | ExtFunApp Id [Id]
    deriving (Show)

data KFunDef = 
    FunDef { name :: (Id, T.Type), 
             args :: [(Id, T.Type)], 
             body :: KNorm }
    deriving (Show)

fv :: KNorm -> Set.Set String
fv Unit = Set.empty
fv (Int _) = Set.empty
fv (Float _) = Set.empty
fv (Neg x) = Set.singleton x
fv (Add x y) = Set.fromList [x,y]
fv (Sub x y) = Set.fromList [x,y]
fv (FNeg x) = Set.singleton x
fv (FAdd x y) = Set.fromList [x,y]
fv (FSub x y) = Set.fromList [x,y]
fv (FMul x y) = Set.fromList [x,y]
fv (FDiv x y) = Set.fromList [x,y]
fv (IfEq x y e1 e2) = 
    Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (IfLE x y e1 e2) = 
    Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (Let (x,_) e1 e2) = Set.union (fv e1) (Set.delete x $ fv e2)
fv (Var x) = Set.singleton x
fv (LetRec (FunDef (x,_) yts e1) e2) = 
    let zs = Set.difference (fv e1) (Set.fromList $ map fst yts) in
        Set.difference (Set.union zs (fv e2)) (Set.singleton x)
fv (App x ys) = Set.fromList $ x:ys
fv (Tuple xs) = Set.fromList xs
fv (LetTuple xts y e) = 
    Set.insert y $ Set.difference (fv e) (Set.fromList $ map fst xts)
fv (Get x y) = Set.fromList [x,y]
fv (Put x y z) = Set.fromList [x,y,z]
fv (ExtArray _) = Set.empty
fv (ExtFunApp _ xs) = Set.fromList xs

_gentmp :: T.Type -> StateT Integer IO Id
_gentmp t = do
    c <- get
    let (x, c1) = gentmp t c
    put c1
    return x


-- State (...)으로 충분하지만
-- StateT Integer IO (...)으로 하여
-- 필요한 경우 liftIO (putStrLn ...)으로 디버깅할 수 있도록!

insertLet :: (KNorm, T.Type) -> (Id -> StateT Integer IO (KNorm, T.Type)) 
               -> StateT Integer IO (KNorm, T.Type)
insertLet (Var x, t) k = k x
insertLet (e, t) k = do
    x <- _gentmp t
    (e1, t1) <- k x
    return (Let (x, t) e e1, t1)

toKNormal :: E.Exp -> Map.Map Id T.Type -> StateT Integer IO (KNorm, T.Type)
toKNormal E.Unit _ = return (Unit, T.Unit)
toKNormal (E.Int i) _ = return (Int i, T.Int)
toKNormal (E.Float d) _ = return (Float d, T.Float)
toKNormal (E.Not e) env = 
    toKNormal (E.If e (E.Bool False) (E.Bool True)) env
toKNormal (E.Neg e) env = do
    (e1, t) <- toKNormal e env
    insertLet (e1, t) (\x -> return (Neg x, T.Int))
toKNormal (E.Add e1 e2) env = do
    (e1', t1) <- toKNormal e1 env
    (e2', t2) <- toKNormal e2 env
    insertLet (e1', t1) 
        (\x -> insertLet (e2', t2) 
                    (\y -> return (Add x y, T.Int)))
toKNormal (E.Sub e1 e2) env = do
    (e1', t1) <- toKNormal e1 env
    (e2', t2) <- toKNormal e2 env
    insertLet (e1', t1) 
        (\x -> insertLet (e2', t2) 
                    (\y -> return (Sub x y, T.Int)))
toKNormal (E.FNeg e) env = do
    (e1, t) <- toKNormal e env
    insertLet (e1, t) (\x -> return (FNeg x, T.Float))
toKNormal (E.FAdd e1 e2) env = do
    (e1', t1) <- toKNormal e1 env
    (e2', t2) <- toKNormal e2 env
    insertLet (e1', t1) 
        (\x -> insertLet (e2', t2) 
                    (\y -> return (FAdd x y, T.Float)))
toKNormal (E.FSub e1 e2) env = do
    (e1', t1) <- toKNormal e1 env
    (e2', t2) <- toKNormal e2 env
    insertLet (e1', t1) 
        (\x -> insertLet (e2', t2) 
                    (\y -> return (FSub x y, T.Float)))
toKNormal (E.FMul e1 e2) env = do
    (e1', t1) <- toKNormal e1 env
    (e2', t2) <- toKNormal e2 env
    insertLet (e1', t1) 
        (\x -> insertLet (e2', t2) 
                    (\y -> return (FMul x y, T.Float)))
toKNormal (E.FDiv e1 e2) env = do
    (e1', t1) <- toKNormal e1 env
    (e2', t2) <- toKNormal e2 env
    insertLet (e1', t1) 
        (\x -> insertLet (e2', t2) 
                    (\y -> return (FDiv x y, T.Float)))
toKNormal cmp@(E.Eq e1 e2) env = do
    toKNormal (E.If cmp (E.Bool True) (E.Bool False)) env
toKNormal cmp@(E.LE e1 e2) env = do
    toKNormal (E.If cmp (E.Bool True) (E.Bool False)) env
toKNormal (E.If (E.Not e1) e2 e3) env = do
    toKNormal (E.If e1 e3 e2) env
toKNormal (E.If (E.Eq e1 e2) e3 e4) env = do
    (e1', t1) <- toKNormal e1 env
    (e2', t2) <- toKNormal e2 env
    (e3', t3) <- toKNormal e3 env
    (e4', _t4) <- toKNormal e4 env
    insertLet (e1', t1) 
        (\x -> insertLet (e2', t2) 
                    (\y -> return (IfEq x y e3' e4', t3)))
toKNormal (E.If (E.LE e1 e2) e3 e4) env = do
    (e1', t1) <- toKNormal e1 env
    (e2', t2) <- toKNormal e2 env
    (e3', t3) <- toKNormal e3 env
    (e4', _t4) <- toKNormal e4 env
    insertLet (e1', t1) 
        (\x -> insertLet (e2', t2) 
                    (\y -> return (IfLE x y e3' e4', t3)))      
toKNormal (E.If e1 e2 e3) env = do
    toKNormal (E.If (E.Eq e1 (E.Bool False)) e3 e2) env
toKNormal (E.Let (x,t) e1 e2) env = do
    (e1', _t1) <- toKNormal e1 env
    (e2', t2) <- toKNormal e2 (Map.insert x t env)
    return (Let (x,t) e1' e2', t2)
toKNormal (E.Var x) env 
    | Just t <- Map.lookup x env = return (Var x, t)
    | otherwise = error $ "unbound variable: " ++ x -- Todo: extenv!
toKNormal (E.LetRec E.Fundef {E.name=(x,t), E.args=yts, E.body=e1} e2) env = do
    let env' = Map.insert x t env
    (e2', t2) <- toKNormal e2 env'
    (e1', _t1) <- toKNormal e1 (Map.union (Map.fromList yts) env')
    return (LetRec (FunDef (x,t) yts e1') e2', t2)
toKNormal (E.App e es) env = undefined -- Todo: extenv!

toKNormal _ _ = undefined