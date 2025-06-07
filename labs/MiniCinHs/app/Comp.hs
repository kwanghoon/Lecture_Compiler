{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module Comp where 

import Expr
import UCode

type UCodeUnit = [UCInstr]

compile :: TranslationUnit -> (Env, Offset, [DeclInfo])
compile tu = comp tu EmptyEnv 1 1  -- level 1, offset 1

data DeclInfo =
    MemDeclInfo [(UCInstr, DeclSpec, Maybe Int)] -- Memory declarations
  | FuncDeclInfo (DeclSpec, FuncName, [(UCInstr, DeclSpecifier)], [UCInstr]) -- Function declarations
  deriving (Show, Eq)

data Env = 
    EmptyEnv   
  | ExtendEnv String (UCInstr, DeclSpec, Maybe Int) Env 
  | ExtendFuncEnv String (DeclSpec, FuncName, [(UCInstr,DeclSpecifier)], [UCInstr]) Env
  deriving (Show, Eq) 

-- Todo: Environment handling!!

comp :: TranslationUnit -> Env -> Level -> Offset -> (Env, Offset, [DeclInfo])
comp [] env level offset = (env, offset, [])
comp (DeclExtDecl d : theRest) env level offset = 
    let (env1, offset1, memdeclinfo) = compDecl d env level offset 
        (env2, offset2, declinfos) = comp theRest env1 level offset1
    in
        (env2, offset2, MemDeclInfo memdeclinfo : declinfos)
comp (FuncDefExtDecl f : theRest) env level offset = 
    let (env1, offset1, fundefinfo) = compFuncDefHeader f env level offset 
        (env2, offset2, declinfos) = comp theRest env1 level offset1
    in 
        (env2, offset2, FuncDeclInfo fundefinfo : declinfos)

-- Sect. 10.4.2 선언문
compDecl :: Decl -> Env -> Level -> Offset -> (Env, Offset, [(UCInstr, DeclSpec, Maybe Int)])
compDecl (spec, initDecls) env level offset 
    | isConstIntOrInt spec = compInitDeclarators env level offset spec initDecls
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

compInitDeclarators :: Env -> Level -> Offset -> DeclSpec -> [InitDeclarator] -> 
    (Env, Offset, [(UCInstr, DeclSpec, Maybe Int)])
compInitDeclarators env lvl offset spec [] = (env, offset, [])

compInitDeclarators env lvl offset spec (DeclItem (SimpleVar x) maybeInitVal : rest) 
    | uninitializedConst spec maybeInitVal =
        error $ "constant variable " ++ x ++ " must be initialized"
    | otherwise =
        let declitem = (UCsym lvl offset 1, spec, maybeStrToInt maybeInitVal)
            env1 = ExtendEnv x declitem env 

            (env2, offset1, initDeclarators1) = 
                compInitDeclarators env1 lvl (offset+1) spec rest 
        in
            (env2, offset1, declitem : initDeclarators1)

compInitDeclarators env lvl offset spec (DeclItem (ArrayVar x (Just idxStr)) _ : rest) =
    let size = read idxStr :: Int 
        declitem = (UCsym lvl offset size, spec, Nothing)
        env1 = ExtendEnv x declitem env
        -- Todo: handle array initialization
        (env2, offset1, initDeclarators1) = compInitDeclarators env1 lvl (offset+size) spec rest
    in (env2, offset1, declitem : initDeclarators1)
            
compInitDeclarators env lvl offset spec (DeclItem (ArrayVar x Nothing) _ : rest) =
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
    let (_, _, memdeclinfo) = compile tu1 in
        memdeclinfo == [ MemDeclInfo
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
    let (_, _, memdeclinfo) = compile tu2 in
        memdeclinfo == [ MemDeclInfo
                        [(UCsym 1 1 1,[ConstQualifier,IntSpecifier],Just 10),
                        (UCsym 1 2 5,[ConstQualifier,IntSpecifier],Nothing)] ]

-- Sect. 10.4.5 함수
compFuncDefHeader :: FuncDef -> Env -> Level -> Offset
    -> (Env, Offset, (DeclSpec, FuncName, [(UCInstr,DeclSpecifier)], [UCInstr]))
compFuncDefHeader (spec, fname, params, stmt) env level offset 
    | isIntOrVoid spec = 
        let -- foldl :: (b -> a -> b) -> b -> [a] -> b
            (env3, _, memdeclinfo3) = 
                foldl (\triple decl -> 
                        let (env1, offset1, memdeclinfo1) = triple
                            (env2, offset2, memdeclinfo2) = compDecl decl env1 (level+1) offset1
                        in  (env2, offset2, memdeclinfo1 ++ memdeclinfo2) )
                    (env1, 1, []) (parmsToDecl params)    -- env1 for recursion
            funcdefinfo = (spec, fname, 
                [ (ucinstr, head spec) | (ucinstr, spec, _) <- memdeclinfo3 ],
                ucinstrs )
            env1 = ExtendFuncEnv fname funcdefinfo env
            (_,_,ucinstrs)= compStmt stmt env1 level offset
        in ( env1, offset, funcdefinfo )
            
    | otherwise = error $ "invalid or unsupported function return type" ++ show spec

isIntOrVoid :: DeclSpec -> Bool
isIntOrVoid [IntSpecifier] = True
isIntOrVoid [VoidSpecifier] = True
isIntOrVoid _ = False

parmsToDecl :: ParamDeclList -> [Decl]
parmsToDecl params = 
    [ (spec, [DeclItem declarator Nothing]) | ParamDecl spec declarator <- params]

--
--
decl3 :: FuncDef
decl3 = ( [IntSpecifier], "f", 
          [ParamDecl [IntSpecifier] (SimpleVar "x"), 
           ParamDecl [IntSpecifier] (SimpleVar "y"),
           ParamDecl [IntSpecifier] (ArrayVar "a" (Just "5"))
           ], CompoundStmt [] [] )

tu3 :: TranslationUnit
tu3 = [FuncDefExtDecl decl3]

test3 = 
    compile tu3

--
tu4 :: TranslationUnit
tu4 = [DeclExtDecl decl1,
        DeclExtDecl decl2, 
        FuncDefExtDecl decl3,
        DeclExtDecl decl4]   

decl4 :: Decl
decl4 = ( [IntSpecifier], 
          [DeclItem (SimpleVar "w") (Just "123")] )

test4 = 
    compile tu4 
        ==
            (
            ExtendEnv "w" 
                (UCsym 1 15 1, [IntSpecifier], Just 123)
                (
                ExtendFuncEnv "f"
                    (
                    [IntSpecifier],
                    "f",
                    [ (UCsym 2 1 1, IntSpecifier)
                    , (UCsym 2 2 1, IntSpecifier)
                    , (UCsym 2 3 5, IntSpecifier)
                    ],
                    [
                    ]
                    )
                    (
                    ExtendEnv "a"
                        (UCsym 1 10 5, [ConstQualifier, IntSpecifier], Nothing)
                        (
                        ExtendEnv "x"
                            (UCsym 1 9 1, [ConstQualifier, IntSpecifier], Just 10)
                            (
                            ExtendEnv "z"
                                (UCsym 1 8 1, [IntSpecifier], Nothing)
                                (
                                ExtendEnv "a"
                                    (UCsym 1 3 5, [IntSpecifier], Nothing)
                                    (
                                    ExtendEnv "y"
                                        (UCsym 1 2 1, [IntSpecifier], Nothing)
                                        (
                                        ExtendEnv "x"
                                            (UCsym 1 1 1, [IntSpecifier], Just 10)
                                            EmptyEnv
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
            16,
            [ MemDeclInfo
                [ (UCsym 1 1 1, [IntSpecifier], Just 10)
                , (UCsym 1 2 1, [IntSpecifier], Nothing)
                , (UCsym 1 3 5, [IntSpecifier], Nothing)
                , (UCsym 1 8 1, [IntSpecifier], Nothing)
                ],
                MemDeclInfo
                [ (UCsym 1 9 1, [ConstQualifier, IntSpecifier], Just 10)
                , (UCsym 1 10 5, [ConstQualifier, IntSpecifier], Nothing)
                ],
                FuncDeclInfo
                ( [IntSpecifier],
                    "f",
                    [ (UCsym 2 1 1, IntSpecifier)
                    , (UCsym 2 2 1, IntSpecifier)
                    , (UCsym 2 3 5, IntSpecifier)
                    ],
                    [
                    ]
                ),
                MemDeclInfo
                [ (UCsym 1 15 1, [IntSpecifier], Just 123)
                ]
            ]
            )        

-- 
compStmt :: Stmt -> Env -> Level -> Offset -> (Env, Offset, [UCInstr])
compStmt (CompoundStmt declList stmtList) env level offset = (env, offset, [])
compStmt (ExprStmt maybeExpr) env level offset = (env, offset, [])
compStmt (IfStmt expr stmt maybeStmt) env level offset = (env, offset, [])
compStmt (WhileStmt expr stmt) env level offset = (env, offset, [])
compStmt (ReturnStmt maybeExpr) env level offset = (env, offset, [])
