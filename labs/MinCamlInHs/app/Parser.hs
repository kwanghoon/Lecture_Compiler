module Parser(parserSpec) where

import Attrs
import CommonParserUtil
import Token
-- import Expr

import ParserTime

-- | Utility
rule :: String -> b -> (String, b, Maybe a2)
rule prodRule action              = (prodRule, action, Nothing  )

ruleWithPrec :: String -> String -> b -> (String, b, Maybe String)
ruleWithPrec prodRule prec action = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token () IO () -- AST
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
      rule "Start -> Exp" (\rhs -> return $ get rhs 1),

      -- SimpleExp: 
      rule "SimpleExp -> ( Exp )" (\_rhs -> undefined),
      rule "SimpleExp -> ( )" (\_rhs -> undefined),
      rule "SimpleExp -> true" (\_rhs -> undefined),
      rule "SimpleExp -> false" (\_rhs -> undefined),
      rule "SimpleExp -> int" (\_rhs -> undefined),
      rule "SimpleExp -> float" (\_rhs -> undefined),
      rule "SimpleExp -> ident" (\_rhs -> undefined),
      rule "SimpleExp -> SimpleExp . ( Exp )" (\_rhs -> undefined),

      -- Exp: 
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

      -- FunDef 
      rule "FunDef -> ident FormalArgs = Exp" (\_rhs -> undefined),

      -- FormalArgs
      rule "FormalArgs -> ident FormalArgs" (\_rhs -> undefined),
      rule "FormalArgs -> ident" (\_rhs -> undefined),

      -- ActualArgs
      ruleWithPrec "ActualArgs -> ActualArgs SimpleExp" 
        {- %prec -} "prec_app"
        (\_rhs -> undefined),
      ruleWithPrec "ActualArgs -> SimpleExp" 
        {- %prec -} "prec_app"
        (\_rhs -> undefined),

      -- Elems
      rule "Elems -> Elems , Exp" (\_rhs -> undefined),
      rule "Elems -> Exp , Exp" (\_rhs -> undefined),

      -- Pat 
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