module Expr where
import Distribution.Simple.Configure (ConfigStateFileError(ConfigStateFileNoHeader))
import Data.Type.Bool (If)
import Distribution.Simple.Program.GHC (GhcProfAuto(GhcProfAutoExported))

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

