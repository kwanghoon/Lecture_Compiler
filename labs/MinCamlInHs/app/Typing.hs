module Typing(tychecker) where

import Type ( Type(..) )
import Syntax ( Exp(..) ) 
import qualified Data.Map as Map

tychecker :: Exp -> Type
tychecker _ = Type.Int

type TyEnv = Map.Map String Type 
type Subst = Map.Map Integer Type


tyChk :: Exp -> TyEnv -> Subst -> (Type, Subst)
tyChk = undefined 

