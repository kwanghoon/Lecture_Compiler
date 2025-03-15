module Closure (C(..), Prog(..), cloconv, fv) where 


import Id (TlvId(..))
import qualified Type as T
import qualified KNormal as K
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.Trans.State ( StateT, get, put, runStateT )

type Id = String

type Closure = (TlvId, [Id])

data C =
    Unit
  | Int Int 
  | Float Double
  | Neg Id
  | Add Id Id
  | Sub Id Id
  | FNeg Id
  | FAdd Id Id
  | FSub Id Id
  | FMul Id Id
  | FDiv Id Id
  | IfEq Id Id C C
  | IfLE Id Id C C
  | Let (Id, T.Type) C C
  | Var Id
  | MakeCls (Id, T.Type) Closure C
  | AppCls Id [Id]
  | AppDir TlvId [Id]
  | Tuple [Id]
  | LetTuple [(Id, T.Type)] Id C
  | Get Id Id
  | Put Id Id Id
  | ExtArray TlvId
  deriving (Show)

type FunDef = ((TlvId, T.Type), [(Id, T.Type)], [(Id,T.Type)], C)

data Prog = Prog [FunDef] C deriving (Show)

fv :: C -> Set.Set Id
fv Unit = Set.empty
fv (Int _) = Set.empty
fv (Float _) = Set.empty
fv (Neg x) = Set.singleton x
fv (Add x y) = Set.fromList [x, y]
fv (Sub x y) = Set.fromList [x, y]
fv (FNeg x) = Set.singleton x
fv (FAdd x y) = Set.fromList [x, y]
fv (FSub x y) = Set.fromList [x, y]
fv (FMul x y) = Set.fromList [x, y]
fv (FDiv x y) = Set.fromList [x, y]
fv (IfEq x y e1 e2) = 
    Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (IfLE x y e1 e2) = 
    Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (Let (x, _) e1 e2) =
    Set.union (fv e1) (Set.delete x (fv e2))
fv (Var x) = Set.singleton x
fv (MakeCls (x, _) (_, ys) e) = 
    Set.union (Set.fromList ys) 
      (Set.difference (fv e) (Set.singleton x))
fv (AppCls x ys) = Set.fromList (x : ys)
fv (AppDir _ xs) = Set.fromList xs
fv (Tuple xs) = Set.fromList xs
fv (LetTuple xts y e) = 
    Set.insert y $ Set.difference (fv e) (Set.fromList $ map fst xts)
fv (Get x y) = Set.fromList [x, y]
fv (Put x y z) = Set.fromList [x, y, z]
fv (ExtArray _) = Set.empty

type TopLevel = [FunDef]
type CCMonad a = StateT TopLevel IO a 
-- The inner monad, IO, is for debugging purposes.

new :: FunDef -> CCMonad ()
new fundef = do
    toplevel <- get
    let toplevel1 = toplevel ++ [fundef]
    put toplevel1
    return ()

cloconv :: K.KNorm ->IO Prog
cloconv k = do
    (cck,toplevels) <- runStateT (cc k Map.empty) [] 
    return (Prog toplevels cck)

cc :: K.KNorm -> Map.Map Id T.Type -> CCMonad C
cc K.Unit _env = return Unit
cc (K.Int i) _env = return (Int i)
cc (K.Float d) _env = return (Float d)
cc (K.Neg x) _env = return (Neg x)
cc (K.Add x y) _env = return (Add x y)
cc (K.Sub x y) _env = return (Sub x y)
cc (K.FNeg x) _env = return (FNeg x)
cc (K.FAdd x y) _env = return (FAdd x y)
cc (K.FSub x y) _env = return (FSub x y)
cc (K.FMul x y) _env = return (FMul x y)
cc (K.FDiv x y) _env = return (FDiv x y)
cc (K.IfEq x y e1 e2) env = do
    cce1 <- cc e1 env 
    cce2 <- cc e2 env 
    return (IfEq x y cce1 cce2)
cc (K.IfLE x y e1 e2) env = do
    cce1 <- cc e1 env
    cce2 <- cc e2 env
    return (IfLE x y cce1 cce2)
cc (K.Let (x, t) e1 e2) env = do
    let env1 = Map.insert x t env
    cce1 <- cc e1 env1
    cce2 <- cc e2 env1
    return (Let (x, t) cce1 cce2)      
cc (K.Var x) _env = return (Var x)
cc (K.LetRec (x, t) yts e1 e2) env = do
    let env1 = Map.insert x t 
                 (foldl (\env0 (y,t0) -> Map.insert y t0 env0) env yts)
    let env2 = Map.insert x t env
    cce1 <- cc e1 env1
    cce2 <- cc e2 env2
    let fvcce1 = Set.difference (fv cce1)
                    (Set.fromList (x : map fst yts))
    let zs = Set.toList fvcce1   
    let zts = map (\z -> case Map.lookup z env1 of 
                           Just t1 -> (z,t1)
                           Nothing -> error ("cconv: Not found: " 
                                               ++ z ++ " in " ++ show env1)) zs
    let fundef = ((TlvId x, t), yts, zts, cce1)
    let closure = (TlvId x, zs)
    new fundef
    return (MakeCls (x, t) closure cce2)
cc (K.App x ys) _env = return (AppCls x ys)
cc (K.Tuple xs) _env = return (Tuple xs)
cc (K.LetTuple xts y e) env = do
    let env1 = foldl (\env0 (x, t) -> Map.insert x t env0) env xts
    cce <- cc e env1
    return (LetTuple xts y cce)
cc (K.Get x y) _env = return (Get x y)
cc (K.Put x y z) _env = return (Put x y z)
cc (K.ExtArray x) _env = return (ExtArray (TlvId x))
cc (K.ExtFunApp x ys) _env = 
    return (AppDir (TlvId ("min_caml" ++ x)) ys)