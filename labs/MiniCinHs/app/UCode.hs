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
  | UCldi          -- ldi
  | UCldp          -- ldp (load parameter)

  | UCstr Int Int  -- str 1 3 
  | UCsti          -- sti
  | UCujp String   -- ujp loop
  | UCtjp String   -- tjp exit
  | UCfjp String   -- fjp next

  | UCcall String  -- call write
  | UCret          -- ret
  | UCretv         -- retv
  | UCchkh 
  | UCchkl

  | UCnop  String      -- label: nop
  | UCproc Int Int Int -- proc 5 2 2 (size of params, block #, level)
  | UCend              -- end
  | UCbgn Int          -- bgn 2 (size of variables)
  | UCsym Int Int Int  -- sym 1 2 1 (level offset size)

  | UCdump 
  | UCnone
  deriving (Show, Eq) 
