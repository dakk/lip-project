exception AsmException of string;;

let bind f x v = fun y -> if y=x then v else f y;;


(*** REGISTERS ***)

let init_reg = fun x -> 0;;

let write_reg regs i v = 
  if i="0" then raise (AsmException "Register 0 cannot be written")
  else bind regs i v;;

let read_reg regs i = regs i;;


(*** MEMORY ***)

let init_mem = fun x -> 0;;

let write_mem mem i v = 
  if i<0 || i>0x10000 then raise (AsmException "Memory index out of bounds")
  else bind mem i v;;

let read_mem mem i = 
  if i<0 || i>0x10000 then raise (AsmException "Memory index out of bounds")
  else mem i;;


(*** PROGRAMS ***)

let rec labels = function
    [] -> []
  | (l,istr)::p -> if (l<>"") then l::(labels p) else labels p
;;

let rec dup = function
    [] -> false
  | l::cont -> (List.mem l cont) || (dup cont)
;;

let rec range a b = if a>b then [] else a::(range (a+1) b);;

let join f g = fun x -> match (f x, g x) with
  (None,v) -> v
| (v,None) -> v
| (Some v, Some w) -> if v=w then Some v else failwith "join";;

(* fun_of_prog: constructs a pair of functions from an ASM program
   fi: (partial) function from indices to instructions 
   fi: (partial) function from labels to indices 
*)

let fun_of_prog p =  
  let n = List.length p
  in let p' = List.combine (range 0 (n-1)) p
  in let fi = List.fold_right (fun (n,(l,istr)) f -> bind f n (Some istr)) p' (fun i -> None)
  in let fl = List.fold_right (fun (n,(l,istr)) f -> if l="" then f else bind f l (Some n)) p' (fun l -> None)
  in (fi,fl)
;;


let index_of_label fl l = match fl l with 
  Some i -> i 
| None -> raise (AsmException ("Unknown label" ^ l));;

let asm_exec_istr (fi,fl) (regs,mem,i) = match fi i with 
  None -> raise (AsmException ("No ASM instruction at index " ^ (string_of_int i)))
| Some istr -> (match istr with
    AsmHalt -> (regs, mem, -1)
  | AsmNop -> (regs, mem, i+1)
  | AsmAdd (a, b, c) -> (write_reg regs a ((read_reg regs b) + (read_reg regs c)), mem, i+1)
  | AsmAddi (a, v, c) -> (write_reg regs a (v + (read_reg regs c)), mem, i+1)
  | AsmSub (a, b, c) -> (write_reg regs a ((read_reg regs b) - (read_reg regs c)), mem, i+1)
  | AsmMul (a, b, c) -> (write_reg regs a ((read_reg regs b) * (read_reg regs c)), mem, i+1)
  | AsmLoad (a,b,off) -> (write_reg regs a (read_mem mem ((read_reg regs b) + (read_reg regs off))),mem,i+1)
  | AsmStore (b,off,a) -> (regs, write_mem mem ((read_reg regs b) + (read_reg regs off)) (read_reg regs a),i+1)
  | AsmJmp l -> (regs, mem, index_of_label fl l)
  | AsmBeq (a, b, l) -> let i' = if (read_reg regs a) = (read_reg regs b) then index_of_label fl l else i+1 in (regs,mem,i')
  | AsmBne (a, b, l) -> let i' = if (read_reg regs a) <> (read_reg regs b) then index_of_label fl l else i+1 in (regs,mem,i')
  | AsmSlt (a, b, c) -> let v = if (read_reg regs b) < (read_reg regs c) then 1 else 0 in (write_reg regs a v, mem, i+1))
;;

let rec loop p f x = if p x then x else loop p f (f x);;

let asm_exec_prog (AsmProg p) =
  if dup (labels p) then raise (AsmException "Duplicate labels")
  else let (fi,fl) = fun_of_prog p in
  loop (fun (_,_,i) -> i=(-1)) (fun x -> asm_exec_istr (fi,fl) x) (init_reg,init_mem,0)
;;


let p0 = AsmProg
         [("",     AsmAddi ("n", 5, "0")); 
          ("",     AsmAddi ("f", 1, "0")); 
	  ("",     AsmAddi ("i", 1, "0")); 
	  ("Loop", AsmSlt ("t","n","i")); 
	  ("",     AsmBne ("t","0","End")); 
	  ("",     AsmMul ("f", "f", "i")); 
	  ("",     AsmAddi ("i", 1, "i"));
	  ("",     AsmJmp "Loop");  
	  ("End",  AsmHalt)];;

let get_regs_istr = function 
   AsmHalt -> []
  | AsmNop -> []
  | AsmAdd (a, b, c) -> union [a] (union [b] [c])
  | AsmAddi (a, v, c) -> union [a] [c]
  | AsmSub (a, b, c) -> union [a] (union [b] [c])
  | AsmMul (a, b, c) ->  union [a] (union [b] [c])
  | AsmLoad (a,b,off) ->  union [a] (union [b] [off])
  | AsmStore (b,off,a) ->  union [a] (union [b] [off])
  | AsmJmp l ->  []
  | AsmBeq (a, b, l) -> union [a] [b]
  | AsmBne (a, b, l) -> union [a] [b]
  | AsmSlt (a, b, c) -> union [a] (union [b] [c])
;;

let rec get_regs = function 
    [] -> []
  | (l,i)::p -> union (get_regs_istr i) (get_regs p)
;;

let rec dump_mem m (l0,l1) = 
  if l0>l1 then []
  else (l0,read_mem m l0)::(dump_mem m (l0+1,l1))
;;

let asm_dump_prog (AsmProg p) d = 
  let (regs,mem,idx)  = asm_exec_prog (AsmProg p)
  in let rl = intersect (fst d) (setminus "0" (get_regs p))
  in let rdump = List.map (fun r -> (r,regs r)) rl
  in let mdump = List.flatten (List.map (dump_mem mem) (snd d))
  in (rdump,mdump)
;;
