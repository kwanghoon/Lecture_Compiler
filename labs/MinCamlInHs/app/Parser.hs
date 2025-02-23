module Parser(parserSpec, expFrom) where

import Attrs
import CommonParserUtil
import Token
import Syntax

import ParserTime

-- | Utility
rule :: String -> b -> (String, b, Maybe a2)
rule prodRule action              = (prodRule, action, Nothing  )

ruleWithPrec :: String -> String -> b -> (String, b, Maybe String)
ruleWithPrec prodRule prec action = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token PET IO () -- AST
parserSpec = ParserSpec
  {
    startSymbol = "Start",

    tokenPrecAssoc =
    [ (Attrs.Nonassoc, [ "in" ])
    , (Attrs.Right,    [ "prec_let" ])
    , (Attrs.Right,    [ ";" ])
    , (Attrs.Right,    [ "prec_if" ])
    , (Attrs.Right,    [ "<-" ])
    , (Attrs.Nonassoc, [ "prec_tuple" ])
    , (Attrs.Left,     [ "," ])
    , (Attrs.Left,     [ "=", "<>", "<", ">", "<=", ">=" ])
    , (Attrs.Left,     [ "+", "-", "+.", "-." ])
    , (Attrs.Left,     [ "*.", "/." ])
    , (Attrs.Right,    [ "prec_unary_minus" ])
    , (Attrs.Left,     [ "prec_app" ])
    , (Attrs.Left,     [ "." ])
    ],

    parserSpecList =
    [
      -- Start :: Exp
      rule "Start -> Exp" (\rhs -> return $ get rhs 1),

      -- SimpleExp :: Exp 
      rule "SimpleExp -> ( Exp )" 
        (\rhs -> return $ get rhs 2),
      rule "SimpleExp -> ( )" 
        (\_rhs -> return $ fromExp Unit),
      rule "SimpleExp -> true" 
        (\_rhs -> return $ fromExp (Bool True)),
      rule "SimpleExp -> false" 
        (\_rhs -> return $ fromExp (Bool False)),
      rule "SimpleExp -> int" 
        (\rhs -> return $ fromExp (Int (read (getText rhs 1) :: Int))),
      rule "SimpleExp -> float" 
        (\rhs -> return $ fromExp (Float (read (getText rhs 1) :: Double))),
      rule "SimpleExp -> ident" 
        (\rhs -> return $ fromExp (Var (getText rhs 1))),
      rule "SimpleExp -> SimpleExp . ( Exp )" 
        (\rhs -> return $ fromExp (Get (expFrom (get rhs 1)) (expFrom (get rhs 4)))),

      -- Exp :: Exp 
      rule "Exp -> SimpleExp" (\_rhs -> undefined),
      ruleWithPrec "Exp -> not Exp" 
        {- %prec -} "prec_app" 
        (\_rhs -> undefined),
      ruleWithPrec "Exp -> - Exp" 
        {- %prec -} "prec_unary_minus"
        (\_rhs -> undefined),
      rule "Exp -> Exp + Exp" (\_rhs -> undefined),
      rule "Exp -> Exp - Exp" (\_rhs -> undefined),
      rule "Exp -> Exp = Exp" (\_rhs -> undefined),
      rule "Exp -> Exp <> Exp" (\_rhs -> undefined),
      rule "Exp -> Exp < Exp" (\_rhs -> undefined),
      rule "Exp -> Exp > Exp" (\_rhs -> undefined),
      rule "Exp -> Exp <= Exp" (\_rhs -> undefined),
      rule "Exp -> Exp >= Exp" (\_rhs -> undefined),
      ruleWithPrec "Exp -> if Exp then Exp else Exp" 
        {- %prec -} "prec_if"
        (\_rhs -> undefined),
      ruleWithPrec "Exp -> -. Exp" 
        {- %prec -} "prec_unary_minus"
        (\_rhs -> undefined),
      rule "Exp -> Exp +. Exp" (\_rhs -> undefined),
      rule "Exp -> Exp -. Exp" (\_rhs -> undefined),
      rule "Exp -> Exp *. Exp" (\_rhs -> undefined),
      rule "Exp -> Exp /. Exp" (\_rhs -> undefined),
      ruleWithPrec "Exp -> let ident = Exp in Exp"
        {- %prec -} "prec_let"
        (\_rhs -> undefined),
      ruleWithPrec "Exp -> let rec FunDef in Exp" 
        {- %prec -} "prec_let"
        (\_rhs -> undefined),
      ruleWithPrec "Exp -> SimpleExp ActualArgs" 
        {- %prec -} "prec_app"
        (\_rhs -> undefined),
      ruleWithPrec "Exp -> Elems" 
        {- %prec -} "prec_tuple"
        (\_rhs -> undefined),
      rule "Exp -> let ( Pat ) = Exp in Exp" (\_rhs -> undefined),
      rule "Exp -> SimpleExp . ( Exp ) <- Exp" (\_rhs -> undefined),
      rule "Exp -> Exp ; Exp" (\_rhs -> undefined),
      ruleWithPrec "Exp -> Array.create SimpleExp SimpleExp" 
        {- %prec -} "prec_app"
        (\_rhs -> undefined),
      rule "Exp -> error" (\_rhs -> undefined),

      -- FunDef :: FunDef
      rule "FunDef -> ident FormalArgs = Exp" (\_rhs -> undefined),

      -- FormalArgs :: [String]
      rule "FormalArgs -> ident FormalArgs" (\_rhs -> undefined),
      rule "FormalArgs -> ident" (\_rhs -> undefined),

      -- ActualArgs :: [Exp]
      ruleWithPrec "ActualArgs -> ActualArgs SimpleExp" 
        {- %prec -} "prec_app"
        (\_rhs -> undefined),
      ruleWithPrec "ActualArgs -> SimpleExp" 
        {- %prec -} "prec_app"
        (\_rhs -> undefined),

      -- Elems :: [Exp]
      rule "Elems -> Elems , Exp" (\_rhs -> undefined),
      rule "Elems -> Exp , Exp" (\_rhs -> undefined),

      -- Pat :: [Ident]
      rule "Pat -> Pat , ident" (\_rhs -> undefined),
      rule "Pat -> ident , ident" (\_rhs -> undefined)
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_mincaml.txt",  
    gotoTblFile    = "goto_table_mincaml.txt",
    grammarFile    = "prod_rules_mincaml.txt",
    parserSpecFile = "mygrammar_mincaml.grm",
    genparserexe   = "yapb-exe",

    synCompSpec = Nothing,
    parserTime = ParserTime {
                   pa_startTime=startTime,
                   pa_finishTime=finishTime
                 }
  }

-- Parsed Expression tree 

data PET = 
    PETExp { expFrom :: Exp }
  | PETFundef { fundefFrom :: Fundef }
  | PETFormalArgs { formalArgsFrom :: [String] }
  | PETActualArgs { actualArgsFrom :: [Exp] }
  | PETElems { elemsFrom :: [Exp] }
  | PETPats { patsFrom :: [Ident] }
  deriving (Show,Eq)

fromExp :: Exp -> PET
fromExp exp = PETExp exp

fromFundef :: Fundef -> PET
fromFundef fundef = PETFundef fundef

fromFormalArgs :: [Ident] -> PET
fromFormalArgs formalArgs = PETFormalArgs formalArgs

fromActualArgs :: [Exp] -> PET
fromActualArgs actualArgs = PETActualArgs actualArgs

fromElems :: [Exp] -> PET
fromElems elems = PETElems elems 

fromPats :: [Ident] -> PET
fromPats pats = PETPats pats 
