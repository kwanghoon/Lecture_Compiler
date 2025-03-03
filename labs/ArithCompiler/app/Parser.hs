module Parser(parserSpec, progFrom) where

import Prelude hiding (exp)

import CommonParserUtil
import Token
import Expr
import ParserState

import ParserTime

-- | Utility
rule prodRule action = (prodRule, action, Nothing)

--
parserSpec :: ParserSpec Token PET IO ParserState -- AST
parserSpec = ParserSpec
  {
    startSymbol = "SeqExpr'",

    tokenPrecAssoc =
      [ 
      ],

    parserSpecList =
    [
      -- Start :: Expr
      rule "SeqExpr' -> SeqExpr" (\rhs -> return $ get rhs 1),


      -- SeqExpr :: [Expr]
      rule "SeqExpr -> SeqExpr ; AssignExpr" 
        (\rhs -> return $ fromExprList 
                            (exprListFrom (get rhs 1) ++ [ exprFrom (get rhs 3)])),

      rule "SeqExpr -> AssignExpr" 
        (\rhs -> return $ fromExprList [exprFrom (get rhs 1)]),

      -- Exp :: Exp 
      rule "AssignExpr -> identifier = identifier" 
        (\rhs -> return $ fromExpr 
                            ( Assign (getText rhs 1) (Var (getText rhs 3)) ))
    ],
    
    baseDir        = "./",
    actionTblFile  = "arith_action_table.txt",  
    gotoTblFile    = "arith_goto_table.txt",
    grammarFile    = "arith_prod_rules.txt",
    parserSpecFile = "arith_mygrammar.grm",
    genparserexe   = "yapb-exe",

    synCompSpec = Nothing,
    parserTime = ParserTime {
                   pa_startTime=startTime,
                   pa_finishTime=finishTime
                 }
  }

-- Parsed Expression tree 
data PET = 
    PETExpr { exprFrom :: Expr }
  | PETExprList { exprListFrom :: [Expr] }
  deriving (Show,Eq)

fromExpr :: Expr -> PET
fromExpr expr = PETExpr expr

fromExprList :: [Expr] -> PET
fromExprList elems = PETExprList elems 

progFrom :: PET -> [Expr]
progFrom = exprListFrom