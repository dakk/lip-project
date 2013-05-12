let string_of_entry = function
    (x,DConst v) -> "const " ^ x ^ " = " ^ string_of_int v
  | (x,DVar l) -> "var   " ^ x ^ " -> " ^ string_of_int l
  | (x,DArray l) -> "array " ^ x ^ " -> " ^ string_of_int l
  | _ -> failwith "mismatch in string_of_entry"
;;

let rec string_of_symtab = function 
    [] -> "\n"
  | (x,d)::l -> string_of_entry (x,d) ^ "\n" ^ (string_of_symtab l)
;;

let bind' f x v = fun y -> if y=x then v else f y;;

let rec fun_of_revlist = function
    [] -> (fun i -> None)
    | (x,v)::l -> let f = fun_of_revlist l in match v with 
	DVar i | DArray i -> bind' f i (Some x)
      |	_ -> f
;;

(* let f0 = fun_of_revlist [("x",DVar 3); ("n",DConst 5); ("y",DArray 2)];; *)

let rec string_of_mem r addr = function
    [] -> "\n"
  | v::m -> let s = (match r addr with Some x -> " (" ^ x ^ ")" | _ -> "") 
            in (string_of_int addr) ^ ": " ^ (string_of_int v) ^ s ^ "\n" ^ 
            (string_of_mem r (addr+1) m)
;;

let string_of_dump d = 
  "Symbols Table:\n" ^ (string_of_symtab (fst d) ^ 
  "Memory\n" ^ (string_of_mem (fun_of_revlist (fst d)) 0 (snd d)));;
