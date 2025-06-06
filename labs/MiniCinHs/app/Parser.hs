module Parser where

import Attrs
import CommonParserUtil
import Token
import Expr

import ParserTime

import Control.Monad.Trans (lift)
import qualified Control.Monad.Trans.State.Lazy as ST

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule prec action = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token ParseTree IO ()
parserSpec = ParserSpec
  {
    startSymbol = "mini_c",

    tokenPrecAssoc =
    [ (Attrs.Nonassoc, [ "LOWER_THAN_TELSE" ]) -- %token integer_number
    , (Attrs.Nonassoc, [ "else" ])             -- %token else
    ],

    parserSpecList =
    [
      rule "mini_c -> translation_unit"
        (\rhs -> return (get rhs 1)),

      -- translation_unit :: TranslationUnit -- [ExternDecl]
      rule "translation_unit -> external_dcl" 
        (\rhs -> return $ 
          PTTranslationUnit 
            [externDeclFrom (get rhs 1)]),

      rule "translation_unit -> translation_unit external_dcl" 
        (\rhs -> return $ 
          PTTranslationUnit 
            (translationUnitFrom (get rhs 1) 
              ++ [externDeclFrom (get rhs 2)]) ),

      -- externanl_dcl :: ExternDecl 
      rule "external_dcl -> function_def"
        (\rhs -> return $ 
          PTExternDecl 
            (FuncDefExtDecl (funcDefFrom (get rhs 1))) ),

      rule "external_dcl -> declaration"
        (\rhs -> return $ 
          PTExternDecl 
            (DeclExtDecl (declFrom (get rhs 1))) ),

      -- function_def :: FuncDef 
      rule "function_def -> function_header compound_st"
        (\rhs -> return $
          let (declSpec, funcName, paramDeclList) = 
                funcHeaderFrom (get rhs 1) in
            PTFuncDef       
              (declSpec, funcName, paramDeclList, 
                [stmtFrom (get rhs 2)]) ),

      -- function_header :: FuncHeader
      rule "function_header -> dcl_spec function_name formal_param"
        (\rhs -> return $ 
          PTFuncHeader 
            (declSpecFrom (get rhs 1),
             funcNameFrom (get rhs 2),
             paramDeclListFrom (get rhs 3)) ),

      -- dcl_spec :: DeclSpec -- [DeclSpecifier]
      rule "dcl_spec -> dcl_specifiers" 
        (\rhs -> return (get rhs 1)),

      -- dcl_specifiers :: DeclSpec -- [DeclSpecifier]
      rule "dcl_specifiers -> dcl_specifier"
        (\rhs -> return $
          PTDeclSpec [ declSpecifierFrom (get rhs 1) ]),

      rule "dcl_specifiers -> dcl_specifiers dcl_specifier"
        (\rhs -> return $ 
          PTDeclSpec $ 
            declSpecFrom (get rhs 1) 
              ++ [ declSpecifierFrom (get rhs 2) ]),

      -- dcl_specifier :: DeclSpecifier 
      rule "dcl_specifier -> type_qualifier" 
        (\rhs -> return (get rhs 1)),

      rule "dcl_specifier -> type_specifier"
        (\rhs -> return (get rhs 1)),

      -- type_qualifier :: DeclSpecifier
      rule "type_qualifier -> const"
        (\rhs -> return (PTDeclSpecifier ConstQualifier)),

      -- type_specifier :: DeclSpecifier
      rule "type_specifier -> int"
        (\rhs -> return (PTDeclSpecifier IntSpecifier)),

      rule "type_specifier -> void"
        (\rhs -> return (PTDeclSpecifier VoidSpecifier)),

      -- function_name :: String
      rule "function_name -> IDENTIFIER"
        (\rhs -> return (PTFuncName (getText rhs 1))),

      -- formal_param :: ParamDeclList -- [ParamDecl]
      rule "formal_param -> ( opt_formal_param )"
        (\rhs -> return (get rhs 2)),

      -- opt_formal_param :: ParamDeclList -- [ParamDecl]
      rule "opt_formal_param -> formal_param_list"
        (\rhs -> return (get rhs 1)),

      rule "opt_formal_param -> "
        (\rhs -> return (PTParamDeclList [])),

      -- formal_param_list :: ParamDeclList -- [ParamDecl]
      rule "formal_param_list -> param_dcl"
        (\rhs -> return $ 
          PTParamDeclList [paramDeclFrom (get rhs 1)] ),

      rule "formal_param_list -> formal_param_list , param_dcl"
        (\rhs -> return $
          PTParamDeclList $
            paramDeclListFrom (get rhs 1) 
              ++ [paramDeclFrom (get rhs 3)] ),

      -- param_dcl :: ParamDecl 
      rule "param_dcl -> dcl_spec declarator"
        (\rhs -> return $ 
          PTParamDecl $
            ParamDecl 
              (declSpecFrom (get rhs 1)) 
              (declaratorFrom (get rhs 2)) ),

      -- compound_st :: Stmt 
      rule "compound_st -> { opt_dcl_list opt_stat_list }"
        (\rhs -> return $ 
          PTStmt $ 
            CompoundStmt 
              (declListFrom (get rhs 2))
              (stmtListFrom (get rhs 3))),

      -- opt_dcl_list :: DeclList -- [Decl]
      rule "opt_dcl_list -> declaration_list"
        (\rhs -> return (get rhs 1)),

      rule "opt_dcl_list -> " 
        (\rhs -> return $ PTDeclList []),

      -- declaration_list :: [Decl]
      rule "declaration_list -> declaration"
        (\rhs -> return $
          PTDeclList $
            [declFrom (get rhs 1)]),

      rule "declaration_list -> declaration_list declaration"
        (\rhs -> return $ 
          PTDeclList $ 
            declListFrom (get rhs 1)
              ++ [declFrom (get rhs 2)]),

      -- declaration :: Decl 
      rule "declaration -> dcl_spec init_dcl_list ;"
        (\rhs -> return $ 
          PTDecl $
              (declSpecFrom (get rhs 1),
               initDeclaratorListFrom (get rhs 2)) ),

      -- init_dcl_list :: [InitDeclarator]
      rule "init_dcl_list -> init_declarator"
        (\rhs -> return $ 
          PTInitDeclaratorList $ 
            [ initDeclaratorFrom (get rhs 1) ] ),

      rule "init_dcl_list -> init_dcl_list , init_declarator"
        (\rhs -> return $ 
          PTInitDeclaratorList $
            (initDeclaratorListFrom (get rhs 1))
              ++ [ initDeclaratorFrom (get rhs 3) ]),

      -- init_declarator :: InitDeclarator
      rule "init_declarator -> declarator"
        (\rhs -> return $ 
          PTInitDeclarator $ 
            DeclItem (declaratorFrom (get rhs 1)) Nothing),

      rule "init_declarator -> declarator = NUMBER"
        (\rhs -> return $ 
          PTInitDeclarator $ 
            DeclItem (declaratorFrom (get rhs 1)) 
              (Just (getText rhs 3))),

      -- declarator :: Declarator
      rule "declarator -> IDENTIFIER" 
        (\rhs -> return $ 
          PTDeclarator $
            SimpleVar (getText rhs 1)),

      rule "declarator -> IDENTIFIER [ opt_number ]"
        (\rhs -> return $ 
          PTDeclarator $
            ArrayVar (getText rhs 1) (optNumberFrom (get rhs 3))),

      rule "opt_number -> NUMBER" 
        (\rhs -> return $ PTOptNumber (Just (getText rhs 1))),

      rule "opt_number -> "
        (\rhs -> return $ PTOptNumber Nothing),

      -- opt_stat_list :: StmtList -- [Stmt]
      rule "opt_stat_list -> statement_list"
        (\rhs -> return (get rhs 1)),

      rule "opt_stat_list -> " 
        (\rhs -> return (PTStmtList [])),

      -- statement_list :: [Stmt]
      rule "statement_list -> statement"
        (\rhs -> return $ 
          PTStmtList $ [stmtFrom (get rhs 1)]),

      rule "statement_list -> statement_list statement"
        (\rhs -> return $
          PTStmtList $ 
            stmtListFrom (get rhs 1) 
              ++ [stmtFrom (get rhs 2)]),

      -- statement, expression_st :: Stmt
      rule "statement -> compound_st" (\rhs -> return (get rhs 1)),
      rule "statement -> expression_st" (\rhs -> return (get rhs 1)),
      rule "statement -> if_st" (\rhs -> return (get rhs 1)),
      rule "statement -> while_st" (\rhs -> return (get rhs 1)),
      rule "statement -> return_st" (\rhs -> return (get rhs 1)),

      rule "expression_st -> opt_expression ;" 
        (\rhs -> return (PTStmt (ExprStmt (optExprFrom (get rhs 1))))),

      -- opt_expression :: Maybe Expr
      rule "opt_expression -> expression" 
        (\rhs -> return (PTOptExpr (Just (exprFrom (get rhs 1))))),

      rule "opt_expression -> " (\rhs -> return (PTOptExpr Nothing)),

      -- if_st, while_st, return_st :: Stmt
      ruleWithPrec "if_st -> if ( expression ) statement" "LOWER_THAN_TELSE"
        (\rhs -> return $ PTStmt $ 
          IfStmt (exprFrom (get rhs 3)) (stmtFrom (get rhs 5)) Nothing ),

      rule "if_st -> if ( expression ) statement else statement"
        (\rhs -> return $ PTStmt $ 
          IfStmt (exprFrom (get rhs 3))
            (stmtFrom (get rhs 5))
            (Just (stmtFrom (get rhs 7))) ),

      rule "while_st -> while ( expression ) statement"
        (\rhs -> return $ PTStmt $ 
          WhileStmt (exprFrom (get rhs 3)) (stmtFrom (get rhs 5)) ),

      rule "return_st -> return opt_expression ;"
        (\rhs -> return $ PTStmt $ ReturnStmt (optExprFrom (get rhs 2))),

      -- expression, assignment_exp, ... :: Expr
      rule "expression -> assignment_exp" (\rhs -> return (get rhs 1)),

      rule "assignment_exp -> logical_or_exp" (\rhs -> return (get rhs 1)),
      rule "assignment_exp -> unary_exp = assignment_exp" 
        (\rhs -> return $ PTExpr $ 
          Assign (exprFrom (get rhs 1)) (exprFrom (get rhs 3))),
      rule "assignment_exp -> unary_exp += assignment_exp"
        (\rhs -> return $ PTExpr $ 
          AssignOp "+=" (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),
      rule "assignment_exp -> unary_exp -= assignment_exp"
        (\rhs -> return $ PTExpr $ 
          AssignOp "-=" (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),
      rule "assignment_exp -> unary_exp *= assignment_exp"
        (\rhs -> return $ PTExpr $ 
          AssignOp "*=" (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),
      rule "assignment_exp -> unary_exp /= assignment_exp"
        (\rhs -> return $ PTExpr $ 
          AssignOp "/=" (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),
      rule "assignment_exp -> unary_exp %= assignment_exp"
        (\rhs -> return $ PTExpr $ 
          AssignOp "%=" (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "logical_or_exp -> logical_and_exp" (\rhs -> return (get rhs 1)),

      rule "logical_or_exp -> logical_or_exp || logical_and_exp"
        (\rhs -> return $ PTExpr $ 
          LogicalOr (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "logical_and_exp -> equality_exp" (\rhs -> return (get rhs 1)),

      rule "logical_and_exp -> logical_and_exp && equality_exp"
        (\rhs -> return $ PTExpr $ 
          LogicalAnd (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "equality_exp -> relational_exp" (\rhs -> return (get rhs 1)),

      rule "equality_exp -> equality_exp == relational_exp"
        (\rhs -> return $ PTExpr $ 
          Equal (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "equality_exp -> equality_exp != relational_exp"
        (\rhs -> return $ PTExpr $ 
          NotEqual (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "relational_exp -> additive_exp" (\rhs -> return (get rhs 1)),

      rule "relational_exp -> relational_exp > additive_exp"
        (\rhs -> return $ PTExpr $ 
          GreaterThan (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "relational_exp -> relational_exp < additive_exp"
        (\rhs -> return $ PTExpr $ 
          LessThan (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "relational_exp -> relational_exp >= additive_exp"
        (\rhs -> return $ PTExpr $ 
          GreaterThanOrEqualTo (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "relational_exp -> relational_exp <= additive_exp"
        (\rhs -> return $ PTExpr $ 
          LessThanOrEqualTo (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "additive_exp -> multiplicative_exp" (\rhs -> return (get rhs 1)),

      rule "additive_exp -> additive_exp + multiplicative_exp"
        (\rhs -> return $ PTExpr $ 
          Add (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "additive_exp -> additive_exp - multiplicative_exp"
        (\rhs -> return $ PTExpr $ 
          Sub (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "multiplicative_exp -> unary_exp" (\rhs -> return (get rhs 1)),

      rule "multiplicative_exp -> multiplicative_exp * unary_exp"
        (\rhs -> return $ PTExpr $ 
          Mul (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "multiplicative_exp -> multiplicative_exp / unary_exp"
        (\rhs -> return $ PTExpr $ 
          Div (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "multiplicative_exp -> multiplicative_exp % unary_exp"
        (\rhs -> return $ PTExpr $ 
          Mod (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "unary_exp -> postfix_exp" (\rhs -> return (get rhs 1)),

      rule "unary_exp -> - unary_exp"
        (\rhs -> return $ PTExpr $ 
          UnaryMinus (exprFrom (get rhs 2)) ),

      rule "unary_exp -> ! unary_exp"
        (\rhs -> return $ PTExpr $ 
          LogicalNot (exprFrom (get rhs 2)) ),

      rule "unary_exp -> ++ unary_exp"
        (\rhs -> return $ PTExpr $ 
          PreIncrement (exprFrom (get rhs 2)) ),

      rule "unary_exp -> -- unary_exp"
        (\rhs -> return $ PTExpr $ 
          PreDecrement (exprFrom (get rhs 2)) ),

      rule "postfix_exp -> primary_exp" (\rhs -> return (get rhs 1)),

      rule "postfix_exp -> postfix_exp [ expression ]"
        (\rhs -> return $ PTExpr $ 
          ArrayIndex (exprFrom (get rhs 1)) (exprFrom (get rhs 3)) ),

      rule "postfix_exp -> postfix_exp ( opt_actual_param )"
        (\rhs -> return $ PTExpr $ 
          Call (exprFrom (get rhs 1)) (exprListFrom (get rhs 3)) ),

      rule "postfix_exp -> postfix_exp ++"
        (\rhs -> return $ PTExpr $ 
          PostIncrement (exprFrom (get rhs 1)) ),

      rule "postfix_exp -> postfix_exp --"
        (\rhs -> return $ PTExpr $ 
          PostDecrement (exprFrom (get rhs 1)) ),

      -- opt_actual_param :: [Expr]
      rule "opt_actual_param -> actual_param" (\rhs -> return $ get rhs 1),

      rule "opt_actual_param -> " (\rhs -> return $ PTExprList []),

      -- actual_param, actual_param_list :: [Expr]
      rule "actual_param -> actual_param_list" (\rhs -> return $ get rhs 1),

      rule "actual_param_list -> assignment_exp" 
        (\rhs -> return $ PTExprList $ [ exprFrom (get rhs 1) ]),

      rule "actual_param_list -> actual_param_list , assignment_exp"
        (\rhs -> return $ PTExprList $ 
            exprListFrom (get rhs 1) 
              ++ [ exprFrom (get rhs 3) ]),

      -- primary_exp :: Expr
      rule "primary_exp -> IDENTIFIER"
        (\rhs -> return $ PTExpr $ (Identifier (getText rhs 1))),

      rule "primary_exp -> NUMBER" 
        (\rhs -> return $ PTExpr $ (Number (getText rhs 1))),

      rule "primary_exp -> ( expression )" (\rhs -> return $ get rhs 2)

    ],
    
    baseDir        = "./",
    actionTblFile  = "mini_c_action_table.txt",  
    gotoTblFile    = "mini_c_goto_table.txt",
    grammarFile    = "mini_c_prod_rules.txt",
    parserSpecFile = "mini_c_mygrammar.grm",
    genparserexe   = "yapb-exe",

    synCompSpec = Nothing,
    parserTime = ParserTime {
                   pa_startTime=startTime,
                   pa_finishTime=finishTime
                 }
  }


