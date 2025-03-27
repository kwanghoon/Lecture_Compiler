module Virtual where

import Id(genId,TlvId(..))
import Asm(Seq(..), Exp(..), IdOrImm(..), concatSeq, seq, reghp, align)
import qualified Type as T
import qualified Closure as C
import qualified Data.Map as Map
import qualified Data.List as List

import Control.Monad.State 

type FloatTble = [(TlvId, Double)]
type CompState = (Integer, FloatTble)

type Id = String
type Env = Map.Map Id T.Type

--
classify xts init addf addi = 
    List.foldl f init xts
    where f acc (x, T.Unit) = acc 
          f acc (x, T.Float) = addf acc x
          f acc (x, t) = addi acc x t

separate :: [(a, T.Type)] -> ([a], [a])
separate xts =
    classify 
        xts
        ([], [])
        (\(is, fs) x -> (is, fs++[x]))
        (\(is, fs) x _ -> (is++[x], fs))

expand xts ini addf addi = 
    classify 
        xts
        ini
        (\(offset, acc) x -> (offset+8, addf x offset acc))
        (\(offset, acc) x t -> (offset+4, addi x t offset acc))

--
toVirtual :: C.C -> Env -> State CompState Seq
toVirtual C.Unit _env = return (Ans Nop)
toVirtual (C.Int i) _env = return (Ans (Set i))
toVirtual (C.Float d) _env = do 
    (n, tbl) <- get
    l <- case [ i | (i, f) <- tbl, f == d ] of
          (i:_) -> return i
          [] -> do let l = TlvId (genId "l" n)
                   put (n+1, (l, d):tbl)
                   return l
    (n1,tbl1) <- get
    let x = genId "l" n1
    put (n1+1, tbl1)
    return (Let (x, T.Int) (SetL l) (Ans (LdDF x (C 0) 1)))
toVirtual (C.Neg x) _env = return (Ans (Neg x))
toVirtual (C.Add x y) _env = return (Ans (Add x (V y)))
toVirtual (C.Sub x y) _env = return (Ans (Sub x (V y)))
toVirtual (C.FNeg x) _env = return (Ans (FNegD x))
toVirtual (C.FAdd x y) _env = return (Ans (FAddD x y))
toVirtual (C.FSub x y) _env = return (Ans (FSubD x y))
toVirtual (C.FMul x y) _env = return (Ans (FMulD x y))
toVirtual (C.FDiv x y) _env = return (Ans (FDivD x y))
toVirtual (C.IfEq x y e1 e2) env = do
    case Map.lookup x env of
      Just T.Bool -> do
          e1' <- toVirtual e1 env
          e2' <- toVirtual e2 env
          return (Ans (IfEq x (V y) e1' e2'))
      Just T.Int -> do
          e1' <- toVirtual e1 env
          e2' <- toVirtual e2 env 
          return (Ans (IfEq x (V y) e1' e2'))
      Just T.Float -> do
          e1' <- toVirtual e1 env
          e2' <- toVirtual e2  env
          return (Ans (IfFEq x y e1' e2'))
      _ -> error "equality supported only for bool, int, and float"
toVirtual (C.IfLE x y e1 e2) env = do
    case Map.lookup x env of
      Just T.Bool -> do
          e1' <- toVirtual e1 env
          e2' <- toVirtual e2 env
          return (Ans (IfLE x (V y) e1' e2'))
      Just T.Int -> do
          e1' <- toVirtual e1 env 
          e2' <- toVirtual e2 env 
          return (Ans (IfLE x (V y) e1' e2'))
      Just T.Float -> do
          e1' <- toVirtual e1 env 
          e2' <- toVirtual e2 env
          return (Ans (IfFLE x y e1' e2'))
      _ -> error "inequality supported only for bool, int, and float"
toVirtual (C.Let (x,t) e1 e2) env = do
    e1' <- toVirtual e1 env
    e2' <- toVirtual e2 (Map.insert x t env)
    return $ concatSeq e1' (x,t) e2'
toVirtual (C.Var x) env = 
    case Map.lookup x env of
      Just T.Unit -> return (Ans Nop)
      Just T.Float -> return (Ans (FMovD x))
      _ -> return (Ans (Mov x))
-- toVirtual (C.MakeCls (x, t) (l, ys) e2) env = do
--     e2' <- toVirtual e2 (Map.insert x t env)
--     let (offset, storefv) = 
--             expand 
--                 (List.map (find env) ys)
--                 (4, e2')
--                 (\y offset storefv1 -> Asm.seq (StDF y x (C offset) 1) storefv1) -- Todo: stateful!
--                 (\y _ offset storefv2 -> Asm.seq (St y x (C offset) 1) storefv2)
--     (n, tbl) <- get
--     z <- genId "l" n
--     put (n+1, tbl)
--     return $ Let (x, t) (Mov reghp)
--                 (Let (reghp, T.Int) (Add reghp (C (align offset)))
--                     (Let (z, T.Int) (SetL l)
--                         (Asm.seq (St z x (C 0) 1), storefv)))
--     where
--         find env y =
--             case Map.lookup y env of
--               Just t -> (y, t)
--               Nothing -> error ("Virtual.toVirtual:MakeCls: " ++ y)
