module Interp where

import Expr 

import qualified Data.Map as Map 

evalExpr :: Expr -> Map.Map String Int -> Int 
evalExpr = undefined