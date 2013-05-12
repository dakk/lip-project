(* ocamlc -c -I +camlp4 -pp camlp4of.opt asmi.ml *)
(* camlp4 main.cmo *)

#use "set.ml";;
#use "asm-syntax.ml";;
#use "asm-prettyp.ml";;
#use "asm-interpreter.ml";;
#use "asm-parser.ml";;

let parse_asmline s =
   AsmGram.parse_string asmline (Loc.mk "<string>") s;;

let debug_reg r (dr,dm) = (union [r] dr, dm);;
let debug_mem (l0,l1) (dr,dm) = (dr,union [(l0,l1)] dm);;

let rec parse_loop p n d =
  try 
    let s = read_line () in (match parse_asmline s with
      AsmIstr (l,istr) -> parse_loop ((l,istr)::p) (n+1) d
    | AsmComment c -> parse_loop p (n+1) d
    | AsmDebugReg r -> parse_loop p (n+1) (debug_reg r d)
    | AsmDebugMem (l0,l1) -> parse_loop p (n+1) (debug_mem (l0,l1) d))
  with End_of_file -> (AsmProg (List.rev p),d)
|  Loc.Exc_located (_, x) -> failwith ("Parse error at line " ^ (string_of_int n))
;;

let _ = let (p,d) = parse_loop [] 1 ([],[]) in 
  print_string ("Program:\n" ^ (string_of_asmprog p) ^ "\n"); 
  print_string (string_of_asmdump (asm_dump_prog p d));;
