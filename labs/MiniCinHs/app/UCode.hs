module UCode where 

{- opcode
    notop,     neg,    incop,   decop, dup, swp,  add,  sub,   mult,   divop,
     modop,   andop,    orop,    gt,    lt,     ge,   le,     eq,    ne,
     lod,       ldc,      lda,     ldi,     ldp,  str,    sti,    ujp,   tjp,    fjp,
     call,      ret,      retv,    chkh,    chkl, nop,    proc, endop,   bgn,  sym,
     dump,  none
-}

data UCInstr
  = UCnot         -- not
  | UCneg         -- neg
  | UCinc         -- inc
  | UCdec         -- dec
  | UCdup         -- dup
  | UCswp         -- swp
  
  | UCadd         -- add
  | UCsub         -- sub
  | UCmult        -- mult
  | UCdiv         -- div 
  | UCmod         -- mod

  | UCand 
  | UCor 
  | UCgt           -- gt
  | UClt           -- lt
  | UCge           -- ge
  | UCle           -- le
  | UCeq           -- eq
  | UCne           -- ne

  | UClod Int Int  -- lod 1 1
  | UCldc Int      -- ldc 100
  | UClda Int Int  -- lda 
  | UCldi          -- ldi (load indirect)
  | UCldp          -- ldp (load parameter)

  | UCstr Int Int  -- str 1 3 
  | UCsti          -- sti (store indirect)
  | UCujp String   -- ujp loop
  | UCtjp String   -- tjp exit
  | UCfjp String   -- fjp next

  | UCcall String  -- call write
  | UCret          -- ret
  | UCretv         -- retv
  | UCchkh 
  | UCchkl

  | UCnop  String      -- label: nop

  -- fname  proc 5 2 2 (size of params+local vars, 
  --                    block #,    전역 변수 영역 0, 각 함수별 1, 2, 3, ...
  --                    lexical level) 함수는 항상 2?
  | UCproc String Int Int Int 

  | UCend              -- end
  | UCbgn Int          -- bgn 2 (size of variables)
  | UCsym Int Int Int  -- sym 1 2 1 (base-block #? offset size)

  | UCdump 
  | UCnone
  
  | UCcomment String -- comment for debugging
  deriving (Show, Eq) 

fill :: String -> Int -> String
fill str n = take n (str ++ replicate n ' ')

pprintUCode :: UCInstr -> String
pprintUCode instr = case instr of
  UCproc fname p b l -> 
    fill fname 11
      ++ fill "proc" 8 ++ fill (show p) 8 ++ fill (show b) 8 ++ fill (show l) 8
  UCnop label     -> fill label 11 ++ "nop"
  UCcomment str   -> "# " ++ str
  _               -> replicate 11 ' ' ++ format instr
  where
    format i = case i of
      UCnot           -> "not"
      UCneg           -> "neg"
      UCinc           -> "inc"
      UCdec           -> "dec"
      UCdup           -> "dup"
      UCswp           -> "swp"

      UCadd           -> "add"
      UCsub           -> "sub"
      UCmult          -> "mult"
      UCdiv           -> "div"
      UCmod           -> "mod"

      UCand           -> "and"
      UCor            -> "or"
      UCgt            -> "gt"
      UClt            -> "lt"
      UCge            -> "ge"
      UCle            -> "le"
      UCeq            -> "eq"
      UCne            -> "ne"

      UClod l o       -> fill "lod" 8 ++ fill (show l) 8 ++ fill (show o) 8
      UCldc v         -> fill "ldc" 8 ++ fill (show v) 8
      UClda l o       -> fill "lda" 8 ++ fill (show l) 8 ++ fill (show o) 8
      UCldi           -> "ldi"
      UCldp           -> "ldp"

      UCstr l o       -> fill "str" 8 ++ fill (show l) 8 ++  fill (show o) 8
      UCsti           -> "sti"
      UCujp label     -> fill "ujp" 8 ++ label
      UCtjp label     -> fill "tjp" 8 ++ label
      UCfjp label     -> fill "fjp" 8 ++ label

      UCcall name     -> fill "call" 8 ++ name
      UCret           -> "ret"
      UCretv          -> "retv"
      UCchkh          -> "chkh"
      UCchkl          -> "chkl"

      UCend           -> "end"
      UCbgn size      -> fill "bgn" 8 ++ fill (show size) 8
      UCsym l o s     -> fill "sym" 8 
                          ++ fill (show l) 8 
                          ++ fill (show o) 8 
                          ++ fill (show s) 8

      UCdump          -> "dump"
      UCnone          -> "none"
      _               -> error "Unhandled instruction in format"