module Expr where

import Data.Type.Bool (If)
import Data.Maybe 

data Parse =
    ParseSeq  { fromParseSeq  :: [Expr] } -- Expr Sequence: Expr1; ... ; Exprn
  | ParseExpr { fromParseExpr :: Expr   }

instance Show Parse where
  showsPrec p _ = (++) "Parse ..."

toParseSeq :: [Expr] -> Parse
toParseSeq exprs = ParseSeq exprs

toParseExpr :: Expr -> Parse
toParseExpr expr = ParseExpr expr

-- data Expr =
--     Program
--   | FuncDef 
--   | ConstNode 
--   | IntNode 
--   | VoidNode 
--   | FormalParam
--   | ParamDecl
--   | CompoundStmt 
--   | DeclList 
--   | Decl 
--   | DeclItem 
--   | SimpleVar 
--   | ArrayVar 
--   | StmtList 
--   | ExprStmt 
--   | IfStmt 
--   | WhileStmt 
--   | ReturnStmt 
--   | AssignExpr
--   | AddAssign 
--   | SubAssign 
--   | MulAssign 
--   | DivAssign 
--   | ModAssign 
--   | LogicalOr
--   | LogicalAnd
--   | EqExpr
--   | NeqExpr 
--   | GTExpr 
--   | LTExpr 
--   | GEExpr 
--   | AddExpr 
--   | SubExpr 
--   | MulExpr 
--   | DivExpr 
--   | ModExpr 
--   | UnaryMinusExpr 
--   | LogicalNotExpr 
--   | PreIncExpr 
--   | PreDecExpr
--   | PostIncExpr 
--   | PostDecExpr
--   | IndexExpr 
--   | CallExpr
--   | ActualParam
--   deriving (Show, Eq)

-- [Mini-C AST 구조]
-- data Expr
-- type ExprList = [Expr]
-- data Stmt 
-- type StmtList = [Stmt]
-- data Decl 
-- type DeclList = [Decl]
-- data ParamDecl 
-- type ParamDeclList = [ParamDecl]
-- data ExternDecl 
-- type ExternDeclList = [ExternDecl]
-- type TranslationUnit = ExternDeclList

data Expr =
    Assign Expr Expr
  | AssignOp String Expr Expr -- "+=", "-=", "*=", "/=", "%="
  | Add Expr Expr 
  | Sub Expr Expr 
  | Mul Expr Expr 
  | Div Expr Expr 
  | Mod Expr Expr 
  | LogicalOr Expr Expr
  | LogicalAnd Expr Expr 
  | LogicalNot Expr
  | Equal Expr Expr 
  | NotEqual Expr Expr 
  | GreaterThan Expr Expr
  | LessThan Expr Expr 
  | GreaterThanOrEqualTo Expr Expr 
  | LessThanOrEqualTo Expr Expr 
  | UnaryMinus Expr 
  | PreIncrement Expr 
  | PreDecrement Expr 
  | ArrayIndex Expr Expr 
  | Call Expr ExprList 
  | PostIncrement Expr 
  | PostDecrement Expr 
  | Identifier String 
  | Number String
  deriving (Show, Eq)

type ExprList = [Expr]

data Stmt =
    CompoundStmt DeclList StmtList
  | ExprStmt (Maybe Expr) 
  | IfStmt Expr Stmt (Maybe Stmt)
  | WhileStmt Expr Stmt 
  | ReturnStmt (Maybe Expr)
  deriving (Show, Eq)

type StmtList = [Stmt]

--
type DeclSpec = [DeclSpecifier] -- Not empty

data DeclSpecifier = 
    ConstQualifier 
  | IntSpecifier
  | VoidSpecifier
  deriving (Show, Eq)

data Declarator =
    SimpleVar String
  | ArrayVar String (Maybe String) -- array name, number index
  deriving (Show, Eq)

data InitDeclarator = 
    DeclItem Declarator (Maybe String)
  deriving (Show, Eq)

type Decl = (DeclSpec, [InitDeclarator])
type DeclList = [Decl]

--
data ParamDecl = ParamDecl DeclSpec Declarator
  deriving (Show, Eq)

type ParamDeclList = [ParamDecl]

--
data ExternDecl = 
    FuncDefExtDecl FuncDef
  | DeclExtDecl Decl 
  deriving (Show, Eq)

type FuncName = String
type FuncDef = (DeclSpec, FuncName, ParamDeclList, Stmt)
type FuncHeader = (DeclSpec, FuncName, ParamDeclList)

-- 
type TranslationUnit = [ExternDecl]

-- Parse trees
data ParseTree = 
    PTExpr { exprFrom :: Expr } 
  | PTExprList { exprListFrom :: ExprList }
  | PTOptExpr { optExprFrom :: Maybe Expr}
  | PTStmt { stmtFrom :: Stmt }
  | PTStmtList { stmtListFrom :: StmtList }
  | PTDeclSpec {declSpecFrom :: DeclSpec }
  | PTDeclSpecifier { declSpecifierFrom :: DeclSpecifier }
  | PTDeclarator { declaratorFrom :: Declarator }
  | PTInitDeclarator { initDeclaratorFrom :: InitDeclarator }
  | PTInitDeclaratorList { initDeclaratorListFrom :: [InitDeclarator] }
  | PTDecl { declFrom :: Decl }
  | PTDeclList { declListFrom :: DeclList }
  | PTParamDecl { paramDeclFrom :: ParamDecl }
  | PTParamDeclList { paramDeclListFrom :: [ParamDecl] }
  | PTFuncDef { funcDefFrom :: FuncDef }
  | PTFuncHeader { funcHeaderFrom :: FuncHeader }
  | PTFuncName { funcNameFrom :: String }
  | PTOptNumber { optNumberFrom :: Maybe String }
  | PTExternDecl { externDeclFrom :: ExternDecl }
  | PTTranslationUnit { translationUnitFrom :: TranslationUnit }
  deriving (Show, Eq)




