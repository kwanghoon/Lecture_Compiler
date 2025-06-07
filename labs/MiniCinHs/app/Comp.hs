{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module Comp where 

import Expr
import UCode
import Expr (Decl)

type UCodeUnit = [UCInstr]

compile :: TranslationUnit -> [DeclInfo]
compile tu = comp tu

data DeclInfo =
    MemDeclInfo [(UCInstr, DeclSpec, Maybe Int)] -- Memory declarations
  | FuncDeclInfo [UCInstr] -- Function declarations
  deriving (Show, Eq)

comp :: TranslationUnit -> [DeclInfo]
comp [] = []
comp (DeclExtDecl d : theRest) = MemDeclInfo (compDecl d) : comp theRest
comp (FuncDefExtDecl f : theRest) = FuncDeclInfo (compFuncDefHeader f) : comp theRest

-- Sect. 10.4.2 선언문
compDecl :: Decl -> [(UCInstr, DeclSpec, Maybe Int)]
compDecl (spec, initDecls)
    | isConstIntOrInt spec = compInitDeclarators 1 1 spec initDecls
    | otherwise = error $ "compDecl: unsupported specifier " ++ show spec

isConstIntOrInt :: DeclSpec -> Bool
isConstIntOrInt [IntSpecifier] = True
isConstIntOrInt [ConstQualifier, IntSpecifier] = True
isConstIntOrInt _ = False

uninitializedConst :: DeclSpec -> Maybe a -> Bool
uninitializedConst (ConstQualifier : _) Nothing = True
uninitializedConst (ConstQualifier : _) _ = False
uninitializedConst (_ : theRest) maybeV = uninitializedConst theRest maybeV
uninitializedConst [] _ = False

type Level = Int 
type Offset = Int 

compInitDeclarators :: Level -> Offset -> DeclSpec -> [InitDeclarator] -> 
    [(UCInstr, DeclSpec, Maybe Int)]
compInitDeclarators lvl offset spec [] = []

compInitDeclarators lvl offset spec (DeclItem (SimpleVar x) maybeInitVal : rest) 
    | uninitializedConst spec maybeInitVal =
        error $ "constant variable " ++ x ++ " must be initialized"
    | otherwise =
        (UCsym lvl offset 1, spec, maybeStrToInt maybeInitVal) 
            : compInitDeclarators lvl (offset+1) spec rest

compInitDeclarators lvl offset spec (DeclItem (ArrayVar x (Just idxStr)) _ : rest) =
    let size = read idxStr :: Int in 
        (UCsym lvl offset size, spec, Nothing)  -- Todo: handle array initialization
            : compInitDeclarators lvl (offset+size) spec rest

compInitDeclarators lvl offset spec (DeclItem (ArrayVar x Nothing) _ : rest) =
    error "array without size is not supported"

getIndex :: Maybe String -> Int 
getIndex idxStr = 
    case maybeStrToInt idxStr of
        Nothing -> 0
        Just idx -> idx

maybeStrToInt :: Maybe String -> Maybe Int 
maybeStrToInt Nothing = Nothing
maybeStrToInt (Just str) = Just (read str :: Int)

--
decl1 :: Decl
decl1 = ( [IntSpecifier], 
          [DeclItem (SimpleVar "x") (Just "10"), 
           DeclItem (SimpleVar "y") Nothing, 
           DeclItem (ArrayVar "a" (Just "5")) Nothing,
           DeclItem (SimpleVar "z") Nothing ] )

tu1 :: TranslationUnit
tu1 = [DeclExtDecl decl1]

test1 = 
    compile tu1 
        == [ MemDeclInfo
                [(UCsym 1 1 1,[IntSpecifier],Just 10),
                (UCsym 1 2 1,[IntSpecifier],Nothing),
                (UCsym 1 3 5,[IntSpecifier],Nothing),
                (UCsym 1 8 1,[IntSpecifier],Nothing)] ]

decl2 :: Decl
decl2 = ( [ConstQualifier, IntSpecifier], 
          [DeclItem (SimpleVar "x") (Just "10"),
           DeclItem (ArrayVar "a" (Just "5")) Nothing] )

tu2 :: TranslationUnit
tu2 = [DeclExtDecl decl2]           

test2 = 
    compile tu2 
        == [ MemDeclInfo
                [(UCsym 1 1 1,[ConstQualifier,IntSpecifier],Just 10),
                 (UCsym 1 2 5,[ConstQualifier,IntSpecifier],Nothing)] ]

-- Sect. 10.4.5 함수
compFuncDefHeader :: FuncDef -> [UCInstr]
compFuncDefHeader = undefined