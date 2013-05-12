let string_of_reg r = "$" ^ r;;

let string_of_asmistr istr = match istr with
    AsmHalt -> "halt"
  | AsmNop -> "nop"
  | AsmAdd (a, b, c) -> "add " ^ (string_of_reg a) ^ " " ^ (string_of_reg b) ^ " " ^ (string_of_reg c)
  | AsmAddi (a, v, c) -> "addi " ^ (string_of_reg a) ^ " " ^ (string_of_int v) ^ " " ^ (string_of_reg c)
  | AsmSub (a, b, c) -> "sub " ^ (string_of_reg a) ^ " " ^ (string_of_reg b) ^ " " ^ (string_of_reg c)
  | AsmMul (a, b, c) ->  "mul " ^ (string_of_reg a) ^ " " ^ (string_of_reg b) ^ " " ^ (string_of_reg c)
  | AsmLoad (a,b,off) ->  "load " ^ (string_of_reg a) ^ " " ^ (string_of_reg b) ^ " " ^ (string_of_reg off)
  | AsmStore (b,off,a) ->  "store " ^ (string_of_reg b) ^ " " ^ (string_of_reg off) ^ " " ^ (string_of_reg a)
  | AsmJmp l ->  "jmp " ^ l
  | AsmBeq (a, b, l) -> "beq " ^ (string_of_reg a) ^ " " ^ (string_of_reg b) ^ " " ^ l
  | AsmBne (a, b, l) ->"bne " ^ (string_of_reg a) ^ " " ^ (string_of_reg b) ^ " " ^ l
  | AsmSlt (a, b, c) -> "slt " ^ (string_of_reg a) ^ " " ^ (string_of_reg b) ^ " " ^ (string_of_reg c)
;;

let rec string_of_asmprog (AsmProg p) = match p with
  [] -> ""
| (l,istr)::p -> let s = (if l="" then "\t" else l ^ ":\t") in s ^ (string_of_asmistr istr) ^ "\n" ^ (string_of_asmprog (AsmProg p))
;;

let rec string_of_regdump = function
    [] -> ""
  | (r,v)::d -> let s = (string_of_reg r) ^ " -> " ^ (string_of_int v) ^ "\n"
            in s ^ string_of_regdump d
;;

let rec string_of_memdump = function
    [] -> ""
  | (x,v)::d -> let s = (string_of_int x) ^ " -> " ^ (string_of_int v) ^ "\n"
            in s ^ string_of_memdump d
;;

let string_of_asmdump d = 
  (if fst d <> [] then "Registers:\n" ^ (string_of_regdump (fst d)) ^ "\n" else "") ^
  (if snd d <> [] then "Memory:\n" ^ (string_of_memdump (snd d)) else "");;
