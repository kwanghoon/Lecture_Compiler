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
parserSpec :: ParserSpec Token Parse IO ()
parserSpec = ParserSpec
  {
    startSymbol = "mini_c",

    tokenPrecAssoc =
    [ (Attrs.Nonassoc, [ "LOWER_THAN_TELSE" ]) -- %token integer_number
    , (Attrs.Nonassoc, [ "else" ])             -- %token else
    ],

    parserSpecList =
    [
      rule "mini_c -> translation_unit" undefined,

      rule "translation_unit -> external_dcl" undefined,
      rule "translation_unit -> translation_unit external_dcl" undefined,

      rule "external_dcl -> function_def" undefined,
      rule "external_dcl -> declaration" undefined,

      rule "function_def -> function_header compound_st" undefined,

      rule "function_header -> dcl_spec function_name formal_param" undefined,

      rule "dcl_spec -> dcl_specifiers" undefined,

      rule "dcl_specifiers -> dcl_specifier" undefined,
      rule "dcl_specifiers -> dcl_specifiers dcl_specifier" undefined,

      rule "dcl_specifier -> type_qualifier" undefined,
      rule "dcl_specifier -> type_specifier" undefined,

      rule "type_qualifier -> const" undefined,

      rule "type_specifier -> int" undefined,
      rule "type_specifier -> void" undefined,

      rule "function_name -> IDENTIFIER" undefined,

      rule "formal_param -> ( opt_formal_param )" undefined,

      rule "opt_formal_param -> formal_param_list" undefined,
      rule "opt_formal_param -> " undefined,

      rule "formal_param_list -> param_dcl" undefined,
      rule "formal_param_list -> formal_param_list , param_dcl" undefined,

      rule "param_dcl -> dcl_spec declarator" undefined,

      rule "compound_st -> { opt_dcl_list opt_stat_list }" undefined,

      rule "opt_dcl_list -> declaration_list" undefined,
      rule "opt_dcl_list -> " undefined,

      rule "declaration_list -> declaration" undefined,
      rule "declaration_list -> declaration_list declaration" undefined,

      rule "declaration -> dcl_spec init_dcl_list ;" undefined,

      rule "init_dcl_list -> init_declarator" undefined,
      rule "init_dcl_list -> init_dcl_list , init_declarator" undefined,

      rule "init_declarator -> declarator" undefined,
      rule "init_declarator -> declarator = NUMBER" undefined,

      rule "declarator -> IDENTIFIER" undefined,
      rule "declarator -> IDENTIFIER [ opt_number ]" undefined,

      rule "opt_number -> NUMBER" undefined,
      rule "opt_number -> " undefined,

      rule "opt_stat_list -> statement_list" undefined,
      rule "opt_stat_list -> " undefined,

      rule "statement_list -> statement" undefined,
      rule "statement_list -> statement_list statement" undefined,

      rule "statement -> compound_st" undefined,
      rule "statement -> expression_st" undefined,
      rule "statement -> if_st" undefined,
      rule "statement -> while_st" undefined,
      rule "statement -> return_st" undefined,

      rule "expression_st -> opt_expression ;" undefined,

      rule "opt_expression -> expression" undefined,
      rule "opt_expression -> " undefined,

      ruleWithPrec "if_st -> if ( expression ) statement" "LOWER_THAN_TELSE" undefined,
      rule "if_st -> if ( expression ) statement TELSE statement" undefined,

      rule "while_st -> while ( expression ) statement" undefined,

      rule "return_st -> return opt_expression ;" undefined,

      rule "expression -> assignment_exp" undefined,

      rule "assignment_exp -> logical_or_exp" undefined,
      rule "assignment_exp -> unary_exp = assignment_exp" undefined,
      rule "assignment_exp -> unary_exp += assignment_exp" undefined,
      rule "assignment_exp -> unary_exp -= assignment_exp" undefined,
      rule "assignment_exp -> unary_exp *= assignment_exp" undefined,
      rule "assignment_exp -> unary_exp /= assignment_exp" undefined,
      rule "assignment_exp -> unary_exp %= assignment_exp" undefined,

      rule "logical_or_exp -> logical_and_exp" undefined,
      rule "logical_or_exp -> logical_or_exp || logical_and_exp" undefined,

      rule "logical_and_exp -> equality_exp" undefined,
      rule "logical_and_exp -> logical_and_exp && equality_exp" undefined,

      rule "equality_exp -> relational_exp" undefined,
      rule "equality_exp -> equality_exp == relational_exp" undefined,
      rule "equality_exp -> equality_exp != relational_exp" undefined,

      rule "relational_exp -> additive_exp" undefined,
      rule "relational_exp -> relational_exp > additive_exp" undefined,
      rule "relational_exp -> relational_exp < additive_exp" undefined,
      rule "relational_exp -> relational_exp >= additive_exp" undefined,
      rule "relational_exp -> relational_exp <= additive_exp" undefined,

      rule "additive_exp -> multiplicative_exp" undefined,
      rule "additive_exp -> additive_exp + multiplicative_exp" undefined,
      rule "additive_exp -> additive_exp - multiplicative_exp" undefined,

      rule "multiplicative_exp -> unary_exp" undefined,
      rule "multiplicative_exp -> multiplicative_exp * unary_exp" undefined,
      rule "multiplicative_exp -> multiplicative_exp / unary_exp" undefined,
      rule "multiplicative_exp -> multiplicative_exp % unary_exp" undefined,

      rule "unary_exp -> postfix_exp" undefined,
      rule "unary_exp -> - unary_exp" undefined,
      rule "unary_exp -> ! unary_exp" undefined,
      rule "unary_exp -> ++ unary_exp" undefined,
      rule "unary_exp -> -- unary_exp" undefined,

      rule "postfix_exp -> primary_exp" undefined,
      rule "postfix_exp -> postfix_exp [ expression ]" undefined,
      rule "postfix_exp -> postfix_exp ( opt_actual_param )" undefined,
      rule "postfix_exp -> postfix_exp TINC" undefined,
      rule "postfix_exp -> postfix_exp TDEC" undefined,

      rule "opt_actual_param -> actual_param" undefined,
      rule "opt_actual_param -> " undefined,

      rule "actual_param -> actual_param_list" undefined,

      rule "actual_param_list -> assignment_exp" undefined,
      rule "actual_param_list -> actual_param_list , assignment_exp" undefined,

      rule "primary_exp -> IDENTIFIER" undefined,
      rule "primary_exp -> NUMBER" undefined,
      rule "primary_exp -> ( expression )" undefined

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


