module Expr where

data Parse =
    ParseSeq  { fromParseSeq  :: [Expr] } -- Expr Sequence: Expr1; ... ; Exprn
  | ParseExpr { fromParseExpr :: Expr   }

instance Show Parse where
  showsPrec p _ = (++) "Parse ..."

toParseSeq :: [Expr] -> Parse
toParseSeq exprs = ParseSeq exprs

toParseExpr :: Expr -> Parse
toParseExpr expr = ParseExpr expr

data Expr =
    Lit { fromLit :: Int }
  | Var { fromVar :: String }
  | BinOp { kindFromBinOp :: BinOpKind,
            leftOpFromBinOp :: Expr,
            rightOpFromBinOp :: Expr }
  | Assign { lhsFromAssign :: String,
             rhsFromAssign :: Expr  }

data BinOpKind = ADD | SUB | MUL | DIV

pprintParse :: Parse -> String
pprintParse (ParseSeq exprs) =
  let insSemicolon []         = ""
      insSemicolon [str]      = str
      insSemicolon (str:strs) = str ++ "; " ++ insSemicolon strs
  in insSemicolon (map pprint exprs)
    
pprintParse (ParseExpr expr) = pprint expr

pprint :: Expr -> String
pprint (Lit i) = show i
pprint (Var v) = v
pprint (BinOp Expr.ADD left right) =
  "(" ++ pprint left ++ " + " ++ pprint right ++ ")"
pprint (BinOp Expr.SUB left right) =
  "(" ++ pprint left ++ " - " ++ pprint right ++ ")"
pprint (BinOp Expr.MUL left right) =
  "(" ++ pprint left ++ " * " ++ pprint right ++ ")"
pprint (BinOp Expr.DIV left right) =
  "(" ++ pprint left ++ " / " ++ pprint right ++ ")"
pprint (Assign x expr) =   
  "(" ++ x ++ " = " ++ pprint expr ++ ")"
