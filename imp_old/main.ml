(* ocamlc -c -I +camlp4 -pp camlp4of.opt main.ml *)
(* camlp4 main.cmo *)

#use "imp-syntax.ml";;
#use "imp-env.ml";;
#use "imp-store.ml";;
#use "imp-prettyp.ml";;
#use "imp-interpreter.ml";;
#use "imp-parser.ml";;

let parse_prog s =
   ImpGram.parse_string prog (Loc.mk "<string>") s;;

let rec fold_lines acc = 
  try 
    let s = read_line () in fold_lines (acc ^ s ^ "\n")
  with End_of_file -> acc
;;

let _ = 
    let sprog = fold_lines ""
    in 
    (* print_string sprog; *)
    print_string (string_of_dump (dump_prog (parse_prog sprog)))
;;

