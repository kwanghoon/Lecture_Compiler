module KNormal(KNorm(..), knormal, fv) where

import Id(gentmp)
import qualified Type as T
import qualified Syntax as E
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Control.Monad.Trans.State ( StateT, get, put, evalStateT )

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
    | LetRec (Id, T.Type) [(Id, T.Type)] KNorm KNorm
    | App Id [Id]
    | Tuple [Id]
    | LetTuple [(Id, T.Type)] Id KNorm
    | Get Id Id
    | Put Id Id Id
    | ExtArray Id
    | ExtFunApp Id [Id]
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
fv (LetRec (x,_) yts e1 e2) = 
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

_gentmp :: T.Type -> KNMonad Id
_gentmp t = do
    (extenv, c) <- get
    let (x, c1) = gentmp t c
    put (extenv, c1)
    return x

lookupExtEnv :: Id -> KNMonad (Maybe T.Type)
lookupExtEnv x = do
    (extenv, _) <- get
    return $ Map.lookup x extenv

-- State (...)으로 충분하지만
-- StateT Integer IO (...)으로 하여
-- 필요한 경우 liftIO (putStrLn ...)으로 디버깅할 수 있도록!

-- State ::= ExtEnv * int
type State = (Map.Map Id T.Type, Integer)
type KNMonad a = StateT State IO a

insertLet :: (KNorm, T.Type) -> (Id -> KNMonad (KNorm, T.Type)) 
               -> KNMonad (KNorm, T.Type)
insertLet (Var x, _) k = k x
insertLet (e, t) k = do
    x <- _gentmp t
    (e1, t1) <- k x
    return (Let (x, t) e e1, t1)

knormal :: E.Exp -> Map.Map Id T.Type -> IO KNorm
knormal e extenv = do
    (k,_) <- evalStateT (toKNormal e Map.empty) (extenv, 0) 
    return k

toKNormal :: E.Exp -> Map.Map Id T.Type -> KNMonad (KNorm, T.Type)
toKNormal E.Unit _ = return (Unit, T.Unit)

toKNormal (E.Bool b) _ = 
    return (Int (if b then 1 else 0), T.Int)

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

toKNormal cmp@(E.Eq _ _) env = do
    toKNormal (E.If cmp (E.Bool True) (E.Bool False)) env

toKNormal cmp@(E.LE _ _) env = do
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
    | otherwise = 
        do maybeT <- lookupExtEnv x 
           case maybeT of
            Just t@(T.Array _) -> return (ExtArray x, t)
            _ -> error $ "external variable: " ++ x ++ 
                            " does not have an array type"

toKNormal (E.LetRec (x,t) yts e1 e2) env = do
    let env' = Map.insert x t env
    (e2', t2) <- toKNormal e2 env'
    (e1', _t1) <- toKNormal e1 (Map.union (Map.fromList yts) env')
    return (LetRec (x,t) yts e1' e2', t2)

toKNormal (E.App (E.Var f) es) env
  | Maybe.isNothing (Map.lookup f env) = do
    maybeT <- lookupExtEnv f
    case maybeT of
      Just (T.Fun _ t) -> bind [] es t
      _ -> error $ "external function: " ++ f ++ " does not have a function type."
  where
    bind :: [String] -> [E.Exp] -> T.Type -> KNMonad (KNorm, T.Type)
    bind xs [] t = return (ExtFunApp f xs, t)
    bind xs (e2:e2s) t =
      do (x2, t2) <- toKNormal e2 env
         insertLet (x2, t2) (\y -> bind (xs ++ [y]) e2s t)

toKNormal (E.App e es) env = do
    (e1, t1) <- toKNormal e env
    case t1 of 
        T.Fun _ retty -> insertLet (e1, t1) (\f -> bind [] es f retty)
        _ -> error "function application to a non-function value"
  where
    bind :: [String] -> [E.Exp] -> Id -> T.Type -> KNMonad (KNorm, T.Type)
    bind xs [] f t = return (App f xs, t)
    bind xs (e2:e2s) f t =
      do (x2, t2) <- toKNormal e2 env 
         insertLet (x2, t2) (\x -> bind (xs ++ [x]) e2s f t)

toKNormal (E.Tuple es) env = bind [] [] es
  where
    bind :: [String] -> [T.Type] -> [E.Exp] -> KNMonad (KNorm, T.Type)
    bind xs ts [] = return (Tuple xs, T.Tuple ts)
    bind xs ts (e2:e2s) = 
      do (x2, t2) <- toKNormal e2 env
         insertLet (x2, t2) (\x -> bind (xs ++ [x]) (ts ++ [t2]) e2s)

toKNormal (E.LetTuple xts e1 e2) env = do 
  (e1', t1) <- toKNormal e1 env
  insertLet (e1', t1)
    (\y -> do (e2', t2) <- toKNormal e2 (Map.union (Map.fromList xts) env)
              return (LetTuple xts y e2', t2))

toKNormal (E.Array e1 e2) env = do
  (e1', t1) <- toKNormal e1 env
  insertLet (e1', t1)
    (\x -> do (e2', t2) <- toKNormal e2 env
              insertLet (e2', t2) (\y -> do
                let l = case t2 of
                          T.Float -> "create_float_array"
                          _ -> "create_array"
                return (ExtFunApp l [x, y], T.Array t2)))

toKNormal (E.Get e1 e2) env = do
  (e1', t1) <- toKNormal e1 env
  case t1 of 
    T.Array t -> 
      insertLet (e1', t1)
        (\x -> do (e2', t2) <- toKNormal e2 env
                  insertLet (e2', t2) (\y -> return (Get x y, t)))
    _ -> error "array access to a non-array value"
  
toKNormal (E.Put e1 e2 e3) env = do
  (e1', t1) <- toKNormal e1 env
  case t1 of
    T.Array _ -> 
      insertLet (e1', t1)
        (\x -> do (e2', t2) <- toKNormal e2 env
                  insertLet (e2', t2)
                    (\y -> do (e3', _t3) <- toKNormal e3 env
                              insertLet (e3', _t3)
                                (\z -> return (Put x y z, T.Unit))))
    _ -> error "array update to a non-array value"
