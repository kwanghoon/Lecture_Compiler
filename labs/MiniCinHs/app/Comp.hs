{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module Comp where 

import Expr
import UCode

type UCodeUnit = [UCInstr]

compile :: TranslationUnit -> [UCInstr]
compile tu = 
    let (_,_,declInfos) = compile' tu  -- level 1, offset 1
    in codeGenFunc declInfos ++ codeGenMain declInfos 0

compile' tu = comp tu EmptyEnv 1 1 1

codeGenFunc :: [DeclInfo] -> [UCInstr]
codeGenFunc [] = []
codeGenFunc (MemDeclInfo memdeclinfo : rest) = codeGenFunc rest
codeGenFunc (FuncDeclInfo (_, _, _, ucinstrs) : rest) = ucinstrs ++ codeGenFunc rest

codeGenMain :: [DeclInfo] -> Int -> [UCInstr]
codeGenMain [] n = [UCbgn n, UCldp, UCcall "main", UCend]
codeGenMain (MemDeclInfo memdeclinfos : rest) n = 
    let (s, _)= size memdeclinfos in codeGenMain rest (n+s)
codeGenMain (FuncDeclInfo _ : rest) n =  codeGenMain rest n


data DeclInfo =
    MemDeclInfo [(UCInstr, DeclSpec, Maybe Int)] -- Memory declarations
  | FuncDeclInfo (DeclSpec, FuncName, [(UCInstr, DeclSpecifier)], [UCInstr]) -- Function declarations
  deriving (Show, Eq)

data Env = 
    EmptyEnv   
  | ExtendEnv String (UCInstr, DeclSpec, Maybe Int) Env 
  | ExtendFuncEnv String (DeclSpec, FuncName, [(UCInstr,DeclSpecifier)], [UCInstr]) Env
  deriving (Show, Eq)

applyEnv :: Env -> String -> (Block, Offset)
applyEnv EmptyEnv x = error $ "Not found: " ++ x
applyEnv (ExtendEnv y (UCsym block offset size, _, _) env) x
    | x == y = (block, offset)
    | otherwise = applyEnv env x 
applyEnv (ExtendFuncEnv f _ env) x = applyEnv env x

comp :: TranslationUnit -> Env -> Block -> Level -> Offset -> (Env, Offset, [DeclInfo])
comp [] env block level offset = (env, offset, [])
comp (DeclExtDecl d : theRest) env block level offset = 
    let (env1, offset1, memdeclinfo) = compDecl d env 1 level offset 
        (env2, offset2, declinfos) = comp theRest env1 block level offset1
    in
        (env2, offset2, MemDeclInfo memdeclinfo : declinfos)
comp (FuncDefExtDecl f : theRest) env block level offset = 
    let (env1, offset1, fundefinfo) = compFuncDefHeader f env (block+1) level 1 -- reset offset  
        (env2, offset2, declinfos) = comp theRest env1 (block+1) level offset1
    in 
        (env2, offset2, FuncDeclInfo fundefinfo : declinfos)

-- Sect. 10.4.2 선언문
compDecl :: Decl -> Env -> Block -> Level -> Offset -> (Env, Offset, [(UCInstr, DeclSpec, Maybe Int)])
compDecl (spec, initDecls) env block level offset 
    | isConstIntOrInt spec = compInitDeclarators env block level offset spec initDecls
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

type Block = Int
type Level = Int 
type Offset = Int 

compInitDeclarators :: Env -> Block -> Level -> Offset -> DeclSpec -> [InitDeclarator] -> 
    (Env, Offset, [(UCInstr, DeclSpec, Maybe Int)])
compInitDeclarators env block lvl offset spec [] = (env, offset, [])

compInitDeclarators env block lvl offset spec (DeclItem (SimpleVar x) maybeInitVal : rest) 
    | uninitializedConst spec maybeInitVal =
        error $ "constant variable " ++ x ++ " must be initialized"
    | otherwise =
        let declitem = (UCsym block offset 1, spec, maybeStrToInt maybeInitVal)
            env1 = ExtendEnv x declitem env 

            (env2, offset1, initDeclarators1) = 
                compInitDeclarators env1 block lvl (offset+1) spec rest 
        in
            (env2, offset1, declitem : initDeclarators1)

compInitDeclarators env block lvl offset spec (DeclItem (ArrayVar x (Just idxStr)) _ : rest) =
    let size = read idxStr :: Int 
        declitem = (UCsym block offset size, spec, Nothing)
        env1 = ExtendEnv x declitem env
        -- Todo: handle array initialization
        (env2, offset1, initDeclarators1) = compInitDeclarators env1 block lvl (offset+size) spec rest
    in (env2, offset1, declitem : initDeclarators1)
            
compInitDeclarators env block lvl offset spec (DeclItem (ArrayVar x Nothing) _ : rest) =
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
    let (_, _, memdeclinfo) = compile' tu1 in
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
    let (_, _, memdeclinfo) = compile' tu2 in
        memdeclinfo == [ MemDeclInfo
                        [(UCsym 1 1 1,[ConstQualifier,IntSpecifier],Just 10),
                        (UCsym 1 2 5,[ConstQualifier,IntSpecifier],Nothing)] ]

-- Sect. 10.4.5 함수
compFuncDefHeader :: FuncDef -> Env -> Block -> Level -> Offset
    -> (Env, Offset, (DeclSpec, FuncName, [(UCInstr,DeclSpecifier)], [UCInstr]))
compFuncDefHeader (spec, fname, params, stmt) env block level offset 
    | isIntOrVoid spec = 
        let -- foldl :: (b -> a -> b) -> b -> [a] -> b
            (env3, _, memdeclinfo3) = 
                foldl (\triple decl -> 
                        let (env1, offset1, memdeclinfo1) = triple
                            (env2, offset2, memdeclinfo2) = compDecl decl env1 block (level+1) offset1
                        in  (env2, offset2, memdeclinfo1 ++ memdeclinfo2) )
                    (env1, 1, []) (parmsToDecl params)    -- env1 for recursion
            funcdefinfo = (spec, fname, 
                [ (ucinstr, head spec) | (ucinstr, spec, _) <- memdeclinfo3 ],
                ucinstrs1 )
            env1 = ExtendFuncEnv fname funcdefinfo env
            env4 = ExtendFuncEnv fname funcdefinfo env3
            (_,_,ucinstrs)= compStmt stmt env4 (block, level, offset, 1)
            (n, ucsyms) = size memdeclinfo3
            m = localVarSize ucinstrs
            ucinstrs1 = [UCproc fname (n+m) block (level+1)] ++ ucsyms ++ ucinstrs ++ [UCend] -- Todo: block number?
        in ( env1, offset, funcdefinfo )
            
    | otherwise = error $ "invalid or unsupported function return type" ++ show spec


size :: [(UCInstr, DeclSpec, Maybe Int)] -> (Int, [UCInstr])
size [] = (0, [])
size ((instr@(UCsym _ _ s),_,_) : rest) = 
    let (s', ucinstrs) = size rest in (s + s', instr:ucinstrs)

localVarSize :: [UCInstr] -> Int
localVarSize [] = 0
localVarSize (UCsym _ _ s : rest) = s + localVarSize rest   
localVarSize (_ : rest) = 0

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
    compile' tu3
        ==  ( ExtendFuncEnv
                "f"
                ( [IntSpecifier]
                , "f"
                , [ (UCsym 2 1 1, IntSpecifier)
                , (UCsym 2 2 1, IntSpecifier)
                , (UCsym 2 3 5, IntSpecifier)
                ]
                , [ UCproc "f" 7 1 2
                , UCsym 2 1 1
                , UCsym 2 2 1
                , UCsym 2 3 5
                , UCend
                ]
                )
                EmptyEnv
            , 1
            , [ FuncDeclInfo
                ( [IntSpecifier]
                , "f"
                , [ (UCsym 2 1 1, IntSpecifier)
                    , (UCsym 2 2 1, IntSpecifier)
                    , (UCsym 2 3 5, IntSpecifier)
                    ]
                , [ UCproc "f" 7 1 2
                    , UCsym 2 1 1
                    , UCsym 2 2 1
                    , UCsym 2 3 5
                    , UCend
                    ]
                )
            ]
            )            

--
tu4 :: TranslationUnit
tu4 = [DeclExtDecl decl1,
        DeclExtDecl decl2, 
        FuncDefExtDecl decl3,
        DeclExtDecl decl4,
        FuncDefExtDecl decl5]   

decl4 :: Decl
decl4 = ( [IntSpecifier], 
          [DeclItem (SimpleVar "w") (Just "123")] )

decl5 :: FuncDef
decl5 = ( [IntSpecifier], "g", 
          [ParamDecl [IntSpecifier] (SimpleVar "w"), 
           ParamDecl [IntSpecifier] (ArrayVar "b" (Just "5"))
           ], CompoundStmt [] [] )

test4 = 
    compile' tu4 
        ==
            ( ExtendFuncEnv
                "g"
                ( [IntSpecifier]
                , "g"
                , [ (UCsym 3 1 1, IntSpecifier)
                , (UCsym 3 2 5, IntSpecifier)
                ]
                , [ UCproc "g" 6 1 2
                , UCsym 3 1 1
                , UCsym 3 2 5
                , UCend
                ]
                )
                ( ExtendEnv "w"
                    (UCsym 1 15 1, [IntSpecifier], Just 123)
                    ( ExtendFuncEnv
                        "f"
                        ( [IntSpecifier]
                        , "f"
                        , [ (UCsym 2 1 1, IntSpecifier)
                        , (UCsym 2 2 1, IntSpecifier)
                        , (UCsym 2 3 5, IntSpecifier)
                        ]
                        , [ UCproc "f" 7 1 2
                        , UCsym 2 1 1
                        , UCsym 2 2 1
                        , UCsym 2 3 5
                        , UCend
                        ]
                        )
                        ( ExtendEnv "a"
                            (UCsym 1 10 5, [ConstQualifier, IntSpecifier], Nothing)
                            ( ExtendEnv "x"
                                (UCsym 1 9 1, [ConstQualifier, IntSpecifier], Just 10)
                                ( ExtendEnv "z"
                                    (UCsym 1 8 1, [IntSpecifier], Nothing)
                                    ( ExtendEnv "a"
                                        (UCsym 1 3 5, [IntSpecifier], Nothing)
                                        ( ExtendEnv "y"
                                            (UCsym 1 2 1, [IntSpecifier], Nothing)
                                            ( ExtendEnv "x"
                                                (UCsym 1 1 1, [IntSpecifier], Just 10)
                                                EmptyEnv
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            , 16
            , [ MemDeclInfo
                [ (UCsym 1 1 1, [IntSpecifier], Just 10)
                , (UCsym 1 2 1, [IntSpecifier], Nothing)
                , (UCsym 1 3 5, [IntSpecifier], Nothing)
                , (UCsym 1 8 1, [IntSpecifier], Nothing)
                ]
            , MemDeclInfo
                [ (UCsym 1 9 1, [ConstQualifier, IntSpecifier], Just 10)
                , (UCsym 1 10 5, [ConstQualifier, IntSpecifier], Nothing)
                ]
            , FuncDeclInfo
                ( [IntSpecifier]
                , "f"
                , [ (UCsym 2 1 1, IntSpecifier)
                    , (UCsym 2 2 1, IntSpecifier)
                    , (UCsym 2 3 5, IntSpecifier)
                    ]
                , [ UCproc "f" 7 1 2
                    , UCsym 2 1 1
                    , UCsym 2 2 1
                    , UCsym 2 3 5
                    , UCend
                    ]
                )
            , MemDeclInfo
                [ (UCsym 1 15 1, [IntSpecifier], Just 123)
                ]
            , FuncDeclInfo
                ( [IntSpecifier]
                , "g"
                , [ (UCsym 3 1 1, IntSpecifier)
                    , (UCsym 3 2 5, IntSpecifier)
                    ]
                , [ UCproc "g" 6 1 2
                    , UCsym 3 1 1
                    , UCsym 3 2 5
                    , UCend
                    ]
                )
            ]
            )                  

type UniqLabel = (Block, Level, Offset, Int)

-- Sect. 10.4.4 문장
compStmt :: Stmt -> Env -> UniqLabel -> (Env, UniqLabel, [UCInstr])
compStmt (CompoundStmt declList stmtList) env ul =
    let (block, level, offset, n)= ul
        -- foldl :: (b -> a -> b) -> b -> [a] -> b
        (env3, offset3, memdeclinfo3) = 
            foldl (\triple decl -> 
                    let (env1, offset1, memdeclinfo1) = triple
                        (env2, offset2, memdeclinfo2) = compDecl decl env1 block (level+1) offset1
                    in  (env2, offset2, memdeclinfo1 ++ memdeclinfo2) )
                (env, offset, []) declList

        ucinstrsDecls = [ ucinstr | (ucinstr, _, _) <- memdeclinfo3 ]
        
        ul3 = (block, level, offset3, n)

        (env6, ul6, ucinstrs6) =
            foldl (\triple stmt ->
                    let (env4, ul4, ucinstrs4) = triple
                        (env5, ul5, ucinstrs5) = compStmt stmt env4 ul4
                    in  (env5, ul5, ucinstrs4 ++ ucinstrs5) )
                (env3, ul3, []) stmtList 
    in (env6, ul6, ucinstrsDecls ++ ucinstrs6)

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

-- Sect. 10.4.3 식
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
    in (env2, ul2, ucinstrs1 ++ [UCinc {- , UCdup -} ] ++ ucinstrs2) -- Todo:
compExpr (PreDecrement expr) env ul =
    let (env1, ul1, ucinstrs1) = compExpr expr env ul
        (env2, ul2, ucinstrs2) = compLhsExpr expr env1 ul1
    in (env2, ul2, ucinstrs1 ++ [UCdec {- , UCdup -} ] ++ ucinstrs2) -- Todo:

compExpr (ArrayIndex (Identifier x) expr) env ul =
    let (env1, ul1, ucinstrs1) = compExpr expr env ul
        (block, offset) = applyEnv env1 x
        loadArr = UClda block offset
    in (env1, ul1, ucinstrs1 ++ [loadArr, UCadd, UCldi])  -- load indirect
compExpr (ArrayIndex _ _) env ul =
    error "compExpr: unsupported left-hand side expression"

compExpr (Call (Identifier f@"read") [Identifier x]) env ul =
    let (block, offset) = applyEnv env x 
    in (env, ul, [UCldp, UClda block offset] ++ [UCcall f])
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
    in (env2, ul2, ucinstrs1 ++ [{- UCdup, -} UCinc] ++ ucinstrs2) -- Todo:
compExpr (PostDecrement expr) env ul =
    let (env1, ul1, ucinstrs1) = compExpr expr env ul
        (env2, ul2, ucinstrs2) = compLhsExpr expr env1 ul1
    in (env2, ul2, ucinstrs1 ++ [{- UCdup, -} UCdec] ++ ucinstrs2) -- Todo:

compExpr (Identifier x) env ul =
    let (block, offset) = applyEnv env x in (env, ul, [UClod block offset])
compExpr (Number str) env ul = 
    let n = read str :: Int in (env, ul, [UCldc n])

compBinExpr :: Expr -> Expr -> UCInstr 
    -> Env -> UniqLabel -> (Env, UniqLabel, [UCInstr])
compBinExpr expr1 expr2 ucinstr env ul = 
    let (env1, ul1, ucinstrs1) = compExpr expr1 env ul 
        (env2, ul2, ucinstrs2) = compExpr expr2 env1 ul1 
    in (env2, ul2, ucinstrs1 ++ ucinstrs2 ++ [ucinstr]) 

compUnaryExpr :: Expr -> UCInstr 
    -> Env -> UniqLabel -> (Env, UniqLabel, [UCInstr])
compUnaryExpr expr ucinstr env ul = 
    let (env1, ul1, ucinstrs1) = compExpr expr env ul 
    in (env1, ul1, ucinstrs1 ++ [ucinstr])     

compLhsExpr :: Expr -> Env -> UniqLabel -> (Env, UniqLabel, [UCInstr])
compLhsExpr (Identifier x) env ul = 
    let (block, offset) = applyEnv env x in 
        (env, ul, [UCstr block offset])

compLhsExpr (ArrayIndex (Identifier x) expr) env ul = 
    let (env1, ul1, ucinstrs1) = compExpr expr env ul
        (block, offset) = applyEnv env x
        -- loadArr = UClda block offset
    in (env1, ul1, ucinstrs1 ++ [UClda block offset, UCadd, UCswp] ++ [UCsti])

compLhsExpr _ _ _ = 
    error "compLhsExpr: unsupported left-hand side expression"

compArgs :: ExprList -> Env -> UniqLabel -> (Env, UniqLabel, [UCInstr])
compArgs [] env ul = (env, ul, [])
compArgs (expr : rest) env ul =
    let (env1, ul1, ucinstrs1) = compExpr expr env ul
        (env2, ul2, ucinstrs2) = compArgs rest env1 ul1
    in (env2, ul2, ucinstrs1 ++ ucinstrs2)    

genLabel :: UniqLabel -> (String, UniqLabel)
genLabel (block, level, offset, n) = (lbl, (block, level, offset, n+1))
    where lbl = show block ++ "_" ++ show offset ++ "_" ++ show n

opToUninstrs :: String -> [UCInstr]
opToUninstrs op = 
    case op of
        "+=" -> [UCadd]
        "-=" -> [UCsub]
        "*=" -> [UCmult]
        "/=" -> [UCdiv]
        "%=" -> [UCmod]
        _    -> error $ "Unsupported operator " ++ op