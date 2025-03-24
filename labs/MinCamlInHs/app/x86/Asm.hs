module Asm(Exp(..),IdOrImm(..),Seq(..)) where

import qualified Type as T 
import Id
import qualified Data.Map as Map
import GHC.Natural (xorNatural)

type Id = String

data IdOrImm = V Id | C Int deriving (Show)

data Seq = Ans Exp | Let (Id, T.Type) Exp Seq deriving (Show)

data Exp = 
      Nop
    | Set Int
    | SetL TlvId
    | Mov Id
    | Neg Id
    | Add Id IdOrImm
    | Sub Id IdOrImm
    | Ld Id IdOrImm Int
    | St Id Id IdOrImm Int
    | FMovD Id
    | FNegD Id
    | FAddD Id Id
    | FSubD Id Id
    | FMulD Id Id
    | FDivD Id Id
    | LdDF Id IdOrImm Int
    | StDF Id Id IdOrImm Int
    | Comment String
    -- Virtual instructions
    | IfEq Id IdOrImm Seq Seq
    | IfLE Id IdOrImm Seq Seq
    | IfGE Id IdOrImm Seq Seq
    | IfFEq Id Id Seq Seq
    | IfFLE Id Id Seq Seq
    -- Closure address, integer arguments, and float arguments
    | CallCls Id [Id] [Id]
    | CallDir TlvId [Id] [Id]
    | Save Id Id  -- store register variables values onto stack variables
    | Restore Id  -- restore values from stack variables
    deriving (Show)

-- Function definition: name, arguments, free variables, body, return type
type FunDef = (Id, [Id], [Id], Seq, T.Type)

data Prog = Prog [(TlvId, Double)] [FunDef] Seq 
    deriving (Show)

fletd :: Id -> Exp -> Seq -> Seq
fletd x e1 e2 = Let (x, T.Float) e1 e2

seq :: Exp -> Seq -> Integer -> Seq
seq e s n = Let (x, T.Unit) e s
  where (x,_) = gentmp T.Unit n

regs :: Map.Map Int Id
regs = Map.fromList $ zip [1..] [ "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi" ]

fregs :: Map.Map Int Id
fregs = Map.fromList $ zip [1..]  [ "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7" ]

allregs :: [(Int, Id)]
allregs = Map.toList regs

allfregs :: [(Int, Id)]
allfregs = Map.toList fregs

-- regcl 

regsp :: Id
regsp = "%ebp" -- stack pointer

reghp :: Id
reghp = "min_caml_hp" -- heap pointer

isReg :: Id -> Bool
isReg x = head x == '%' || x == reghp

removeanduniq :: Eq a => [a] -> [a] -> [a]
removeanduniq [] [] = []
removeanduniq xs [] = xs
removeanduniq xs (x:ys) =
    if x `elem` xs then removeanduniq xs ys
    else x : removeanduniq xs ys


fvIdOrImm :: IdOrImm -> [Id]
fvIdOrImm (V x) = [x]
fvIdOrImm _ = []

fvExp :: Exp -> [Id]
fvExp e = case e of
    Nop -> []
    Set _ -> []
    SetL _ -> []
    Mov x -> [x]
    Neg x -> [x]
    Add x y -> x : fvIdOrImm y
    Sub x y -> x : fvIdOrImm y
    Ld x y _ -> x : fvIdOrImm y
    St x y z _ -> x : y : fvIdOrImm z
    FMovD x -> [x]
    FNegD x -> [x]
    FAddD x y -> [x, y]
    FSubD x y -> [x, y]
    FMulD x y -> [x, y]
    FDivD x y -> [x, y]
    LdDF x y _ -> x : fvIdOrImm y
    StDF x y z _ -> x : y : fvIdOrImm z
    Comment _ -> []
    IfEq x y s1 s2 -> x : fvIdOrImm y ++ removeanduniq [] (fvSeq s1 ++ fvSeq s2)
    IfLE x y s1 s2 -> x : fvIdOrImm y ++ removeanduniq [] (fvSeq s1 ++ fvSeq s2)
    IfGE x y s1 s2 -> x : fvIdOrImm y ++ removeanduniq [] (fvSeq s1 ++ fvSeq s2)
    IfFEq x y s1 s2 -> x : y : removeanduniq [] (fvSeq s1 ++ fvSeq s2)
    IfFLE x y s1 s2 -> x : y : removeanduniq [] (fvSeq s1 ++ fvSeq s2)
    CallCls x ys zs -> x : ys ++ zs
    CallDir _ ys zs -> ys ++ zs
    Save x _ -> [x]
    Restore _ -> []

-- internal fv function for seq
fvSeq :: Seq -> [Id]
fvSeq (Ans exp1) = fvExp exp1
fvSeq (Let (x, _t) exp1 s) = fvExp exp1 ++ removeanduniq [x] (fvSeq s)

-- external fv function for seq
fv :: Seq -> [Id]
fv seq1 = removeanduniq [] (fvSeq seq1)

concatSeq :: Seq -> (Id, T.Type) -> Seq -> Seq
concatSeq (Ans e1) xt e2 = Let xt e1 e2
concatSeq (Let yt e1 s) xt e2 = Let yt e1 (concatSeq s xt e2)

align :: Int -> Int
align i = if i `mod` 8 == 0 then i else i + 4