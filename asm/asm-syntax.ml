type reg = string;;   (* "$0" models register set to constant 0 *)
type label = string;; (* empty string models no label *)

type asmistr =
   AsmHalt
 | AsmNop
 | AsmAdd of reg * reg * reg
 | AsmAddi of reg * int * reg
 | AsmSub of reg * reg * reg
 | AsmMul of reg * reg * reg
 | AsmLoad of reg * reg * reg
 | AsmStore of reg * reg * reg
 | AsmJmp of label
 | AsmBne of reg * reg * label
 | AsmBeq of reg * reg * label
 | AsmSlt of reg * reg * reg
;;

type asmprog = AsmProg of (label * asmistr) list;;

type asmline = 
    AsmIstr of label * asmistr 
  | AsmComment of string 
  | AsmDebugReg of reg 
  | AsmDebugMem of int * int
;;
