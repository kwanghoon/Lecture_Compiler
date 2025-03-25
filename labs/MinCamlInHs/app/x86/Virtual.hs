module Virtual where

import Id(genId,TlvId(..))
import Asm(Seq(..), Exp(..), IdOrImm(..))
import qualified Type as T
import qualified Closure as C


import Control.Monad.State 

type FloatTble = [(TlvId, Double)]
type CompState = (Integer, FloatTble)

toVirtual :: C.C -> State CompState Seq
toVirtual C.Unit = return (Ans Nop)
toVirtual (C.Int i) = return (Ans (Set i))
toVirtual (C.Float d) = do 
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
toVirtual (C.Neg x) = return (Ans (Neg x))
toVirtual (C.Add x y) = return (Ans (Add x (V y)))