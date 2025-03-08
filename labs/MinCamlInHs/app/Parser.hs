module Parser(parserSpec, expFrom) where

import Prelude hiding (exp)

import Attrs
import CommonParserUtil
import Token
import Syntax
import Type
import ParserState
import Id 

import Control.Monad.Trans (lift)
import qualified Control.Monad.Trans.State.Lazy as ST
import ParserTime

-- | Utility
rule :: String -> b -> (String, b, Maybe a2)
rule prodRule action              = (prodRule, action, Nothing  )

ruleWithPrec :: String -> String -> b -> (String, b, Maybe String)
ruleWithPrec prodRule prec action = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token PET IO ParserState -- AST
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
        (\_rhs -> return $ fromExp Syntax.Unit),
      rule "SimpleExp -> true" 
        (\_rhs -> return $ fromExp (Syntax.Bool True)),
      rule "SimpleExp -> false" 
        (\_rhs -> return $ fromExp (Syntax.Bool False)),
      rule "SimpleExp -> int" 
        (\rhs -> return $ fromExp (Syntax.Int (read (getText rhs 1) :: Int))),
      rule "SimpleExp -> float" 
        (\rhs -> 
           let read_ n = if last n == '.' then read (n++"0") else read n in
             return $ fromExp (Syntax.Float (read_ (getText rhs 1) :: Double))),
      rule "SimpleExp -> ident" 
        (\rhs -> return $ fromExp (Syntax.Var (getText rhs 1))),
      rule "SimpleExp -> SimpleExp . ( Exp )" 
        (\rhs -> return $ fromExp (Get (expFrom (get rhs 1)) 
                            (expFrom (get rhs 4)))),

      -- Exp :: Exp 
      rule "Exp -> SimpleExp" (\rhs -> return $ get rhs 1),
      ruleWithPrec "Exp -> not Exp" 
        {- %prec -} "prec_app" 
        (\rhs -> return $ fromExp (Not (expFrom (get rhs 2)))),
      ruleWithPrec "Exp -> - Exp" 
        {- %prec -} "prec_unary_minus" (\rhs -> 
          return $ fromExp (Neg (expFrom (get rhs 2)))),
      rule "Exp -> Exp + Exp" (\rhs -> 
          return $ fromExp (Add (expFrom (get rhs 1)) (expFrom (get rhs 3)))),
      rule "Exp -> Exp - Exp" (\rhs -> 
          return $ fromExp (Sub (expFrom (get rhs 1)) (expFrom (get rhs 3)))),
      rule "Exp -> Exp = Exp" (\rhs -> 
          return $ fromExp (Eq (expFrom (get rhs 1)) (expFrom (get rhs 3)))),
      rule "Exp -> Exp <> Exp" (\rhs -> 
          return $ fromExp (Not (Eq (expFrom (get rhs 1)) (expFrom (get rhs 3))))),
      rule "Exp -> Exp < Exp" (\rhs -> 
          return $ fromExp (Not (LE (expFrom (get rhs 3)) (expFrom (get rhs 1))))),
      rule "Exp -> Exp > Exp" (\rhs -> 
          return $ fromExp (Not (LE (expFrom (get rhs 1)) (expFrom (get rhs 3))))),
      rule "Exp -> Exp <= Exp" (\rhs -> 
          return $ fromExp (LE (expFrom (get rhs 1)) (expFrom (get rhs 3)))),
      rule "Exp -> Exp >= Exp" (\rhs -> 
          return $ fromExp (LE (expFrom (get rhs 3)) (expFrom (get rhs 1)))),
      ruleWithPrec "Exp -> if Exp then Exp else Exp" 
        {- %prec -} "prec_if"
        (\rhs -> 
          return $ fromExp (If (expFrom (get rhs 2)) 
                             (expFrom (get rhs 4)) (expFrom (get rhs 6)))),
      ruleWithPrec "Exp -> -. Exp" 
        {- %prec -} "prec_unary_minus"
        (\rhs -> return $ fromExp (FNeg (expFrom (get rhs 2)))),
      rule "Exp -> Exp +. Exp" (\rhs -> 
          return $ fromExp (FAdd (expFrom (get rhs 1)) (expFrom (get rhs 3)))),
      rule "Exp -> Exp -. Exp" (\rhs -> 
          return $ fromExp (FSub (expFrom (get rhs 1)) (expFrom (get rhs 3)))),
      rule "Exp -> Exp *. Exp" (\rhs -> 
          return $ fromExp (FMul (expFrom (get rhs 1)) (expFrom (get rhs 3)))),
      rule "Exp -> Exp /. Exp" (\rhs -> 
          return $ fromExp (FDiv (expFrom (get rhs 1)) (expFrom (get rhs 3)))),
      ruleWithPrec "Exp -> let ident = Exp in Exp"
        {- %prec -} "prec_let"
        (\rhs -> return $ fromExp (Let (getText rhs 2, Type.noType) 
                            (expFrom (get rhs 4)) (expFrom (get rhs 6)))),
      ruleWithPrec "Exp -> let rec FunDef in Exp" 
        {- %prec -} "prec_let"
        (\rhs -> return $ fromExp (LetRec (fundefFrom (get rhs 3)) 
                            (expFrom (get rhs 5)))),
      ruleWithPrec "Exp -> SimpleExp ActualArgs" 
        {- %prec -} "prec_app"
        (\rhs -> return $ fromExp (App (expFrom (get rhs 1)) 
                            (actualArgsFrom (get rhs 2)))),
      ruleWithPrec "Exp -> Elems" 
        {- %prec -} "prec_tuple"
        (\rhs -> return $ fromExp (Syntax.Tuple (elemsFrom (get rhs 1)))),
      rule "Exp -> let ( Pat ) = Exp in Exp" (\rhs -> 
        return $ fromExp (LetTuple (patFrom (get rhs 3))
                            (expFrom (get rhs 6)) (expFrom (get rhs 8)))),
      rule "Exp -> SimpleExp . ( Exp ) <- Exp" (\rhs -> 
        return $ fromExp (Put (expFrom (get rhs 1)) 
                              (expFrom (get rhs 4))
                              (expFrom (get rhs 7)))),
      rule "Exp -> Exp ; Exp" (\rhs ->
        do (s,line,col,text) <- ST.get
           let (n,s') = gentmp Type.Unit s 
           ST.put (s',line,col,text)
           return $ fromExp (Let (n, Type.Unit) 
                                (expFrom (get rhs 1)) 
                                (expFrom (get rhs 3)) )),
      ruleWithPrec "Exp -> Array.create SimpleExp SimpleExp" 
        {- %prec -} "prec_app"
        (\rhs -> return $ fromExp (Syntax.Array (expFrom (get rhs 2)) 
                                         (expFrom (get rhs 3)))),
      rule "Exp -> error" (\_rhs -> 
        do (_,line,col,text) <- ST.get
           lift $ putStrLn $ "error: " ++ " at Line " ++ show line ++ 
                             ", Column " ++ show col
           lift $ putStrLn $ " : " ++ take 77 text  -- 80 columns
           return $ fromExp (Syntax.Int 0)),

      -- FunDef :: Fundef
      rule "FunDef -> ident FormalArgs = Exp" (\rhs -> 
        return $ fromFundef 
                   (Fundef (getText rhs 1, Type.noType)
                           (formalArgsFrom (get rhs 2))
                           (expFrom (get rhs 4)))),

      -- FormalArgs :: [(Ident,Type.Type)]
      rule "FormalArgs -> ident FormalArgs" (\rhs ->
        return $ fromFormalArgs ((getText rhs 1, Type.noType) : formalArgsFrom (get rhs 2))),
      rule "FormalArgs -> ident" (\rhs -> 
        return $ fromFormalArgs [(getText rhs 1, Type.noType)]),

      -- ActualArgs :: [Exp]
      ruleWithPrec "ActualArgs -> ActualArgs SimpleExp" 
        {- %prec -} "prec_app"
        (\rhs -> return $ fromActualArgs 
                           (actualArgsFrom (get rhs 1) ++ [expFrom (get rhs 2)])),
      ruleWithPrec "ActualArgs -> SimpleExp" 
        {- %prec -} "prec_app"
        (\rhs -> return $ fromActualArgs [expFrom (get rhs 1)]),

      -- Elems :: [Exp]
      rule "Elems -> Elems , Exp" (\rhs -> 
        return $ fromElems ( elemsFrom (get rhs 1) ++ [expFrom (get rhs 3)] )),
      rule "Elems -> Exp , Exp" (\rhs -> 
        return $ fromElems [ expFrom (get rhs 1), expFrom (get rhs 3)]),

      -- Pat :: [(Ident,Type)]
      rule "Pat -> Pat , ident" (\rhs -> 
        return $ fromPat ( patFrom (get rhs 1) 
                             ++ [(getText rhs 3, Type.noType)] ) ),
      rule "Pat -> ident , ident" (\rhs ->
        return $ fromPat [ (getText rhs 1, Type.noType), 
                           (getText rhs 3, Type.noType)])
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
  | PETFormalArgs { formalArgsFrom :: [(Ident,Type.Type)] }
  | PETActualArgs { actualArgsFrom :: [Exp] }
  | PETElems { elemsFrom :: [Exp] }
  | PETPat { patFrom :: [(Ident,Type.Type)] }
  deriving (Show,Eq)

fromExp :: Exp -> PET
fromExp = PETExp

fromFundef :: Fundef -> PET
fromFundef = PETFundef

fromFormalArgs :: [(Ident,Type.Type)] -> PET
fromFormalArgs = PETFormalArgs

fromActualArgs :: [Exp] -> PET
fromActualArgs = PETActualArgs

fromElems :: [Exp] -> PET
fromElems = PETElems

fromPat :: [(Ident,Type.Type)] -> PET
fromPat = PETPat
