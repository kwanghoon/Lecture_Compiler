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

applyEnv :: Env -> String -> (Level, Offset)
applyEnv EmptyEnv x = error $ "Not found: " ++ x
applyEnv (ExtendEnv y (UCsym level offset size, _, _) env) x
    | x == y = (level, offset)
    | otherwise = applyEnv env x 
applyEnv (ExtendFuncEnv f _ env) x = applyEnv env x

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
            (_,_,ucinstrs)= compStmt stmt env1 (level, offset, 1)
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

type UniqLabel = (Level, Offset, Int)

-- 
compStmt :: Stmt -> Env -> UniqLabel -> (Env, UniqLabel, [UCInstr])
compStmt (CompoundStmt _ stmtList) env ul =  -- Todo: declList
    let -- foldl :: (b -> a -> b) -> b -> [a] -> b
        (env3, ul3, ucinstrs3) =
            foldl (\triple stmt ->
                    let (env1, ul1, ucinstrs1) = triple
                        (env2, ul2, ucinstrs2) = compStmt stmt env1 ul1 
                    in  (env2, ul2, ucinstrs1 ++ ucinstrs2) )
                (env, ul, []) stmtList 
    in (env3, ul3, ucinstrs3)

compStmt (ExprStmt Nothing) env ul = (env, ul, [])
compStmt (ExprStmt (Just expr)) env ul = 
    let (env1, ul1, ucinstrs1) = compExpr expr env ul in 
    (env1, ul1, UCcomment (show expr) : ucinstrs1)

compStmt (IfStmt expr stmt Nothing) env ul =
    let (env1, ul1, ucinstrsCond) = compExpr expr env ul
        (labelNext, ul1')= genLabel ul1
        (env2, ul2, ucinstrsThen) = compStmt stmt env1 ul1'
        jumpToNextOnFalse = UCfjp labelNext
        labelNextInstr = UCnop labelNext
    in (env2, ul2, 
            ucinstrsCond 
                ++ [jumpToNextOnFalse] 
                ++ ucinstrsThen
                ++ [labelNextInstr])

compStmt (IfStmt expr stmt (Just elseStmt)) env ul =
    let (env1, ul1, ucinstrsCond) = compExpr expr env ul
        (labelElse, ul1')= genLabel ul1
        (env2, ul2, ucinstrsThen) = compStmt stmt env1 ul1'
        (labelNext, ul2') = genLabel ul2 
        jumpToElseOnFalse = UCfjp labelElse
        labelElseInstr = UCnop labelElse
        jumpToNext = UCujp labelNext
        labelNextInstr = UCnop labelNext
        (env3, ul3, ucinstrsElse) = compStmt elseStmt env2 ul2'
    in (env3, ul3, 
            [UCcomment ("if " ++ show expr)] ++   
            ucinstrsCond 
                ++ [jumpToElseOnFalse] 
                ++ ucinstrsThen 
                ++ [jumpToNext]
                ++ [labelElseInstr]
                ++ ucinstrsElse
                ++ [labelNextInstr] )


compStmt (WhileStmt expr stmt) env ul =
    let (labelEntry, ul') = genLabel ul
        labelEntryInstr = UCnop labelEntry 
        (env1, ul1, ucinstrsCond) = compExpr expr env ul'
        (env2, ul2, ucinstrsBody) = compStmt stmt env1 ul1
        (labelExit, ul2') = genLabel ul2 
        labelExitInstr = UCnop labelExit 
        jumpToExitOnFalse = UCfjp labelExit
        jumpToEntry = UCujp labelEntry
    in (env2, ul2', 
            [labelEntryInstr]
                ++ [UCcomment ("while " ++ show expr)]
                ++ ucinstrsCond
                ++ [jumpToExitOnFalse]
                ++ ucinstrsBody
                ++ [jumpToEntry]
                ++ [labelExitInstr])

compStmt (ReturnStmt Nothing) env ul = (env, ul, [UCcomment "return", UCret])
compStmt (ReturnStmt (Just expr)) env ul = 
    let (env1, ul1, ucinstrsVal) = compExpr expr env ul
    in  (env, ul, [UCcomment ( "return " ++ show expr)] ++ ucinstrsVal ++ [UCretv])

compExpr :: Expr -> Env -> UniqLabel -> (Env, UniqLabel, [UCInstr])
compExpr (Assign lhs expr) env ul =
    let (env1, ul1, ucinstrs1) = compExpr expr env ul 
        (env2, ul2, ucinstrs2) = compLhsExpr lhs env1 ul1
    in (env2, ul2, ucinstrs1 ++ ucinstrs2)

compExpr (AssignOp op lhs expr) env ul =
    let rhs = lhs
        (env1, ul1, ucinstrs1) = compExpr rhs env ul
        (env2, ul2, ucinstrs2) = compExpr expr env1 ul1
        (env3, ul3, ucinstrs3) = compLhsExpr lhs env2 ul2
        ucinstrsOp = opToUninstrs op 
    in (env3, ul3, ucinstrs1 ++ ucinstrs2 ++ ucinstrsOp ++ ucinstrs3)

compExpr (Add expr1 expr2) env ul = compBinExpr expr1 expr2 UCadd env ul
compExpr (Sub expr1 expr2) env ul = compBinExpr expr1 expr2 UCsub env ul
compExpr (Mul expr1 expr2) env ul = compBinExpr expr1 expr2 UCmult env ul
compExpr (Div expr1 expr2) env ul = compBinExpr expr1 expr2 UCdiv env ul
compExpr (Mod expr1 expr2) env ul = compBinExpr expr1 expr2 UCmod env ul
compExpr (LogicalOr expr1 expr2) env ul = compBinExpr expr1 expr2 UCor env ul
compExpr (LogicalAnd expr1 expr2) env ul = compBinExpr expr1 expr2 UCand env ul
compExpr (Equal expr1 expr2) env ul = compBinExpr expr1 expr2 UCeq env ul
compExpr (NotEqual expr1 expr2) env ul = compBinExpr expr1 expr2 UCne env ul
compExpr (GreaterThan expr1 expr2) env ul = compBinExpr expr1 expr2 UCgt env ul
compExpr (LessThan expr1 expr2) env ul = compBinExpr expr1 expr2 UClt env ul
compExpr (GreaterThanOrEqualTo expr1 expr2) env ul = compBinExpr expr1 expr2 UCge env ul
compExpr (LessThanOrEqualTo expr1 expr2) env ul = compBinExpr expr1 expr2 UCle env ul

compExpr (UnaryMinus expr) env ul = compUnaryExpr expr UCneg env ul 
compExpr (LogicalNot expr) env ul = compUnaryExpr expr UCnot env ul

compExpr (PreIncrement expr) env ul = 
    let (env1, ul1, ucinstrs1) = compExpr expr env ul
        (env2, ul2, ucinstrs2) = compLhsExpr expr env1 ul1
    in (env2, ul2, ucinstrs1 ++ [UCinc, UCdup] ++ ucinstrs2)
compExpr (PreDecrement expr) env ul =
    let (env1, ul1, ucinstrs1) = compExpr expr env ul
        (env2, ul2, ucinstrs2) = compLhsExpr expr env1 ul1
    in (env2, ul2, ucinstrs1 ++ [UCdec, UCdup] ++ ucinstrs2)

compExpr (ArrayIndex (Identifier x) expr) env ul =
    let (env, ul, ucinstrs) = 
            compLhsExpr (ArrayIndex (Identifier x) expr) env ul
    in (env, ul, ucinstrs ++ [UCldi])  -- load indirect
compExpr (ArrayIndex _ _) env ul =
    error "compExpr: unsupported left-hand side expression"

compExpr (Call (Identifier f@"read") [Identifier x]) env ul =
    let (level, offset) = applyEnv env x 
    in (env, ul, [UCldp, UClda level offset] ++ [UCcall f])
compExpr (Call (Identifier f@"write") exprList) env ul =
    let (env1, ul1, ucinstrs1)= compArgs exprList env ul
    in (env1, ul1, [UCldp] ++ ucinstrs1 ++ [UCcall f])
compExpr (Call (Identifier f@"lf") []) env ul = (env, ul, [UCcall f])
compExpr (Call (Identifier f) exprList) env ul = 
    let (env1, ul1, ucinstrs1)= compArgs exprList env ul
    in (env1, ul1, [UCldp] ++ ucinstrs1 ++ [UCcall f])
compExpr (Call expr exprList) env ul = 
    error $ "unsupported call: " ++ show expr
    
compExpr (PostIncrement expr) env ul =
    let (env1, ul1, ucinstrs1) = compExpr expr env ul
        (env2, ul2, ucinstrs2) = compLhsExpr expr env1 ul1
    in (env2, ul2, ucinstrs1 ++ [UCdup, UCinc] ++ ucinstrs2)
compExpr (PostDecrement expr) env ul =
    let (env1, ul1, ucinstrs1) = compExpr expr env ul
        (env2, ul2, ucinstrs2) = compLhsExpr expr env1 ul1
    in (env2, ul2, ucinstrs1 ++ [UCdup, UCdec] ++ ucinstrs2)

compExpr (Identifier x) env ul =
    let (level, offset) = applyEnv env x in (env, ul, [UClod level offset])
compExpr (Number str) env ul = 
    let n = read str :: Int in (env, ul, [UCldc n])

compBinExpr :: Expr -> Expr -> UCInstr 
    -> Env -> UniqLabel -> (Env, UniqLabel, [UCInstr])
compBinExpr expr1 expr2 ucinstr env ul = 
    let (env1, ul1, ucinstrs1) = compExpr expr1 env ul 
        (env2, ul2, ucinstrs2) = compExpr expr2 env ul1 
    in (env2, ul2, ucinstrs1 ++ ucinstrs2 ++ [ucinstr]) 

compUnaryExpr :: Expr -> UCInstr 
    -> Env -> UniqLabel -> (Env, UniqLabel, [UCInstr])
compUnaryExpr expr ucinstr env ul = 
    let (env1, ul1, ucinstrs1) = compExpr expr env ul 
    in (env1, ul1, ucinstrs1 ++ [ucinstr])     

compLhsExpr :: Expr -> Env -> UniqLabel -> (Env, UniqLabel, [UCInstr])
compLhsExpr (Identifier x) env ul = 
    let (level, offset) = applyEnv env x in 
        (env, ul, [UCstr level offset])

compLhsExpr (ArrayIndex (Identifier x) expr) env ul = 
    let (env1, ul1, ucinstrs1) = compExpr expr env ul
        (level, offset) = applyEnv env x
        loadArr = UClda level offset
    in (env1, ul1, ucinstrs1 ++ [UClda level offset, UCadd, UCsti])

compLhsExpr _ _ _ = 
    error "compLhsExpr: unsupported left-hand side expression"

compArgs :: ExprList -> Env -> UniqLabel -> (Env, UniqLabel, [UCInstr])
compArgs [] env ul = (env, ul, [])
compArgs (expr : rest) env ul =
    let (env1, ul1, ucinstrs1) = compExpr expr env ul
        (env2, ul2, ucinstrs2) = compArgs rest env1 ul1
    in (env2, ul2, ucinstrs1 ++ ucinstrs2)    

genLabel :: UniqLabel -> (String, UniqLabel)
genLabel (level, offset, n) = (lbl, (level, offset, n+1))
    where lbl = show level ++ ":" ++ show offset ++ ":" ++ show n

opToUninstrs :: String -> [UCInstr]
opToUninstrs op = 
    case op of
        "+=" -> [UCadd]
        "-=" -> [UCsub]
        "*=" -> [UCmult]
        "/=" -> [UCdiv]
        "%=" -> [UCmod]
        _    -> error $ "Unsupported operator " ++ op