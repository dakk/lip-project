open Camlp4.PreCast ;;

module AsmGram = MakeGram(Lexer) ;;

let reg = AsmGram.Entry.mk "reg" ;;
let label = AsmGram.Entry.mk "label" ;;
let opt_comment = AsmGram.Entry.mk "opt_comment" ;;
let asmistr = AsmGram.Entry.mk "asmistr" ;;
let asmlistr = AsmGram.Entry.mk "asmlistr" ;;
let asmline = AsmGram.Entry.mk "asmline" ;;

EXTEND AsmGram

  reg:
  [
    [ "$"; s=LIDENT -> s 
    | "$"; n=INT -> n]
  ];

  label:
  [
   [ s=UIDENT -> s]
  ];

  opt_comment: 
  [ 
    [ LIST0 [" "] -> "" 
    | "//"; STRING -> "" ] 
  ];

  asmistr: 
  [ 
    [ ["HALT" | "halt"] -> AsmHalt 
    | ["NOP" | "nop"] -> AsmNop
    | ["ADD" | "add"]; a=reg; b=reg; c=reg -> AsmAdd(a,b,c) 
    | ["ADDI" | "addi"]; a=reg; `INT(v,_); c=reg -> AsmAddi(a,v,c) 
    | ["SUB" | "sub"]; a=reg; b=reg; c=reg -> AsmSub(a,b,c) 
    | ["MUL" | "mul"]; a=reg; b=reg; c=reg -> AsmMul(a,b,c) 
    | ["LOAD" | "load"]; a=reg; b=reg; c=reg -> AsmLoad(a,b,c) 
    | ["STORE" | "store"]; a=reg; b=reg; c=reg -> AsmStore(a,b,c) 
    | ["JMP" | "jmp"]; l=label -> AsmJmp(l) 
    | ["BEQ" | "beq"]; a=reg; b=reg; l=label -> AsmBeq(a,b,l) 
    | ["BNE" | "bne"]; a=reg; b=reg; l=label -> AsmBne(a,b,l) 
    | ["SLT" | "slt"]; a=reg; b=reg; c=reg -> AsmSlt(a,b,c) 
    ]
  ];

  asmlistr:
  [
    [ l=label; ":"; i = asmistr -> (l,i)
    | i = asmistr -> ("",i)
    ]
  ];

  asmline:
  [
    [ l=label; ":"; i = asmistr; c=opt_comment -> AsmIstr (l,i)
    | i = asmistr; c=opt_comment -> AsmIstr ("",i)
    | c=opt_comment -> AsmComment c
    | "DEBUG"; "REG"; r=reg; c=opt_comment -> AsmDebugReg r
    | "DEBUG"; "MEM"; `INT(l0,_); `INT(l1,_); c=opt_comment -> AsmDebugMem (l0,l1)
    ] 
  ];

END ;;
