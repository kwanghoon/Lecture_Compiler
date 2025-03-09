module Typing(tychecker) where

import qualified Type as T
import Syntax
import qualified Data.Map as Map
import EitherState

tychecker :: Exp -> Either String (T.Type, Subst)
tychecker e = 
  _run (tyChk e Map.empty Map.empty)

type TyEnv = Map.Map String T.Type 
type Subst = Map.Map Integer T.Type


tyChk :: Exp -> TyEnv -> Subst -> Either_ String (T.Type, Subst)
-- tyChk Unit _ _ = _Right (T.Unit, Map.empty)
-- tyChk (Bool _) _ _ = _Right (T.Bool, Map.empty)
-- tyChk (Int _) _ _ = _Right (T.Int, Map.empty)
-- tyChk (Float _) _ _ = _Right (T.Float, Map.empty)
-- tyChk (Not e) tyenv subst = do 
--   (t, subst1) <- tyChk e tyenv subst
--   subst2 <- unify T.Bool t subst1
--   return (T.Bool, subst2)
tyChk _ _ _ = undefined 

unify :: T.Type -> T.Type -> Subst -> Either_ String Subst
unify _t1 _t2 _subst = _Left "unify not implemented"

