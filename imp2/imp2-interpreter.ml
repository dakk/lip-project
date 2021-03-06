(************************************************************)
(*                           BIGINTEGERS                    *)
(************************************************************)


(* bigint type *)
type bigint = int list;;

let num_max = 100000000;;

(* bigint operations *)
let rec length = function
  [] -> 0
| x::xl -> 1 + length xl;;

let rec rev l = 
  match l with
    [] -> []
  | x::xl -> (rev xl) @ [x]
;;

let bigint_to_sign l = match l with
    [] -> 0
  | x::l' -> if x > 0 then 1 else if x < 0 then -1 else 0
;;

let rec bigint_abs b = match b with 
    [] -> [] 
  | x::b' -> (if x>=0 then x else x*(-1))::(bigint_abs b')
;;

let bigint_lt l1 l2 = 
  let bigint_lt' b1 b2 = 
    if length b1 < length b2 then true else
    if length b1 > length b2 then false else
    (
      let rec bigint_lt'' b1' b2' = match (b1',b2') with
          (x::r1,y::r2) -> if x<y then true else if x>y then false else (bigint_lt'' r1 r2)
        | (_,_) -> false 
        in bigint_lt'' b1 b2
    ) in match (bigint_to_sign l1,bigint_to_sign l2) with
    (0,0) -> false
  | (1,-1) -> false
  | (-1,1) -> true
  | (-1,0) -> true
  | (0,-1) -> false
  | (1,0) -> false
  | (0,1) -> true
  | (1,1) -> bigint_lt' l1 l2
  | (_,_) -> bigint_lt' l1 l2
;;
  
let rec bigint_abs l = match l with
    [] -> []
  | x::l' -> (if x<0 then x*(-1) else x)::bigint_abs l';;
  
let bigint_bigger l1 l2 =
  if bigint_lt (bigint_abs l2) (bigint_abs l1) then (l1,l2,bigint_to_sign l1,bigint_to_sign l2) else
  (l2,l1,bigint_to_sign l2,bigint_to_sign l1)
;;

let bigint_add l1 l2 = 
  let (maxval,minval,smaxval,sminval) = bigint_bigger l1 l2 in
  let rec bigint_add' b1 b2 r = 
    match (b1,b2) with
        (x::b1',y::b2') -> 
          let (ris,sris) = ((x+y+r),bigint_to_sign [(x+y+r)]) in
          if ((sris = smaxval) || (sris = 0) || (b1' = [])) then
            (ris mod num_max)::bigint_add' b1' b2' (ris / num_max)
          else (((smaxval)*num_max+ris) mod num_max)::bigint_add' b1' b2' ((-1)*smaxval)
      | (x::b1',[]) -> if (((x+r) = 0) && (b1' = [])) then [] else (x+r)::(bigint_add' b1' [] 0)
      | ([],y::b2') -> if (((y+r) = 0) && (b2' = [])) then [] else (y+r)::(bigint_add' [] b2' 0)
      | ([],[]) -> if r=0 then [] else [r]      
    in match (smaxval,sminval) with
          (1,0) -> maxval
        | (0,1) -> minval
        | (-1,0) -> maxval
        | (0,-1) -> minval
        | (_,_) -> rev (bigint_add' (rev maxval) (rev minval) 0)
;;

let bigint_eq l1 l2 = 
  if length l1 != length l2 then false
  else
  (
    let rec eq' b1 b2 = match (b1,b2) with
        (x::b1',y::b2') -> if x!=y then false else eq' b1' b2' 
      | (_,_) -> true
    in eq' l1 l2
  )
;;

let rec bigint2string n =
  let n2pos y = if y>0 then y else y*(-1) in
  let rec bigint2string' b e = match b with
    [] -> "" |
    i::l' -> (
                if e=0 then string_of_int i
                else
                (
                  let i' = n2pos i
                  in
                    if (String.length (string_of_int i')) < (String.length (string_of_int num_max) - 1) 
                    then String.sub (string_of_int (i' + num_max)) 1 ((String.length (string_of_int num_max))-1)
                    else string_of_int i'
                )
              ) ^ (bigint2string' l' (e+1))
    in bigint2string' n 0
;;


(* data types *)
type lval = LBool of int | LInteger of int;;
type mval = Bool of bool | Integer of bigint | Nil;;


(************************************************************)
(*                           ENVIRONMENT                    *)
(************************************************************)

(* env: function from identifiers (ide) to dval *)

type ide = string;;
type loc = int;;

(* dval: semantic domain of environments *)

type dval =
  Unbound
| DConst of mval
| DVar of lval
;;

type env = Env of (ide -> dval);;

exception UnboundIde of ide;;

(* interface with the environment *)

let emptyenv = fun () -> Env (fun x -> Unbound);;

let bind (Env rho) x d = Env (fun y -> if y=x then d else rho y);;

let applyenv (Env rho) x = match rho x with
  Unbound -> raise (UnboundIde x)
| _ as d -> d
;;

(************************************************************)
(*                              STORE                       *)
(************************************************************)

(* store: pair (s,d), where 
          s is function from locations (loc) to mval
          d is an integer (size of the store, set to 2^16)
*)

type store = Store of (loc -> mval) * int;;

(* interface with the store *)
let emptystore = fun () -> Store ((fun x -> Nil), 0x1FFFF);;

exception AddressOutOfBounds of loc;;

let applystore (Store (sigma,dim)) l = 
    if 0 <= l && l < dim then sigma l
    else raise (AddressOutOfBounds l)
;;

let update (Store (sigma,dim)) l m = 
    if 0 <= l && l < dim then Store ((fun l' -> if l'=l then m else sigma l'), dim)
    else raise (AddressOutOfBounds l)
;;


(************************************************************)
(*                           EXPRESSIONS                    *)
(************************************************************)


(* exp: IMP expressions *)

type exp =
  True
| False
| Not of exp
| And of exp * exp
| Or of exp * exp
| Int of bigint
| Add of exp * exp
| Eq of exp * exp
| Lt of exp * exp
| Val of ide
;;


exception TypeMismatch of string;;


(* exp *) 
let rec sem_exp e (r, s) = match e with
  True -> Bool true 
| False -> Bool false
| Not e' ->(match (sem_exp e' (r,s)) with 
                  Bool b -> Bool (not b) 
                | _ -> raise (TypeMismatch ("NotType mismatch")))
| And (e1,e2) -> (match (sem_exp e1 (r,s),sem_exp e2 (r,s)) with
                  (Bool b1,Bool b2) -> Bool (b1 && b2) 
                | _ -> raise (TypeMismatch ("AndType mismatch")))
| Or (e1,e2) -> (match (sem_exp e1 (r,s),sem_exp e2 (r,s)) with
                  (Bool b1,Bool b2) -> Bool (b1 || b2) 
                | _ -> raise (TypeMismatch ("OrType mismatch")))
| Int l1 -> Integer l1
| Add (e1,e2) -> (match (sem_exp e1 (r,s),sem_exp e2 (r,s)) with
                  (Integer l1,Integer l2) -> Integer (bigint_add l1 l2)
                | _ -> raise (TypeMismatch ("AddType mismatch")))
| Eq (e1,e2) -> (match (sem_exp e1 (r,s),sem_exp e2 (r,s)) with
                  (Integer l1,Integer l2) -> Bool (bigint_eq l1 l2)
                | _ -> raise (TypeMismatch ("EqType mismatch")))
| Lt (e1,e2) -> (match (sem_exp e1 (r,s),sem_exp e2 (r,s)) with
                  (Integer l1,Integer l2) -> Bool (bigint_lt l1 l2)
                | _ -> raise (TypeMismatch ("LtType mismatch")))
| Val x -> (match applyenv r x with             
              DConst c -> c
             | DVar l' -> (match l' with
                      LBool b -> applystore s b
                    | LInteger i -> applystore s i)
             | _ -> raise (UnboundIde x)
             )
;;


(************************************************************)
(*                           DECLARATIONS                   *)
(************************************************************)

(* dec: IMP declarations *)

type dec =
    Empty
  | Const of ide * mval
  | VarInt of ide
  | VarBool of ide
  | Dseq of dec * dec
;;

let rec sem_dec d (r,l) = match d with
  Empty -> (r,l)
| Const(x,v) -> (bind r x (DConst v), l)
| VarInt x -> (bind r x (DVar (LInteger l)), l+1)
| VarBool x -> (bind r x (DVar (LBool l)), l+1)
| Dseq(d1,d2) -> sem_dec d2 (sem_dec d1 (r,l))
;;


(************************************************************)
(*                             COMMANDS                     *)
(************************************************************)

(* com: IMP commands *)

type com =
    Skip
  | Break
  | Assign of ide * exp
  | Cseq of com * com
  | If of exp * com * com
  | Repeat of com
;;

type flag = Ok | Br;;

let rec sem_com c (rho,sigma) = match c with
    Skip -> (sigma,Ok)
  | Break -> (sigma,Br)
  | Assign(x,e) -> let e' = (sem_exp e (rho,sigma)) in
      (match applyenv rho x with
        DVar l -> (match l with
                    LInteger i -> (match e' with
                          Integer i' -> (update sigma i (Integer i'),Ok)
                        | _ -> raise (TypeMismatch ("You have tried to assign to non-variable" ^ x)))
                  | LBool b -> (match e' with
                          Bool b' -> (update sigma b (Bool b'),Ok)
                        | _ -> raise (TypeMismatch ("You have tried to assign to non-variable" ^ x))))
      | _ -> raise (TypeMismatch ("You have tried to assign to non-variable" ^ x)))
  | Cseq(c1,c2) -> 
        (let (sigma',flag') = sem_com c1 (rho,sigma) in
          match flag' with
            Ok -> sem_com c2 (rho,sigma')
          | Br -> (sigma',Br))
  | If(e,c1,c2) -> 
      if sem_exp e (rho,sigma) = Bool false
      then sem_com c2 (rho,sigma) 
      else sem_com c1 (rho,sigma)
  | Repeat(c') -> 
        (let (sigma',flag') = sem_com c' (rho,sigma) in
          match flag' with
            Ok -> sem_com c (rho,sigma')
          | Br -> (sigma',Ok))
;;


(************************************************************)
(*                             PROGRAMS                     *)
(************************************************************)

type prog = Program of dec * com;;

let sem_prog (Program (d,c)) =
  let (rho,l) = sem_dec d (emptyenv(),0)
  in fst(sem_com c (rho,emptystore()))
;;

(* functions used for debugging programs *)

let rec range a b = 
  if a>b then []
  else a::(range (a+1) b);;
 
let rec ide = function
    Empty -> []
  | Const (x,v) -> [x]
  | VarInt x -> [x]
  | VarBool x -> [x]
  | Dseq (d1,d2) -> (ide d1) @ (ide d2)
;;
 
let dump (Program (d,c)) = 
  let (r,l) = sem_dec d (emptyenv(),0)
  in let s' = fst(sem_com c (r,emptystore()))
  in (List.map (fun x -> (x,applyenv r x)) (ide d), 
      List.map (applystore s') (range 0 (l-1)))
;;


let dval2string d = match d with 
  DConst i -> (match i with
                Integer e -> bigint2string e |
                Bool e -> if e then "true" else "false" |
                Nil -> "NIL") |
  DVar i -> (match i with 
              LInteger i' -> string_of_int i' |
              LBool b' -> string_of_int b') |
  Unbound -> "Unbound"
;;

let rec printenv l = match l with 
    [] -> () |
    (s,l)::l' -> Printf.printf "\tSymbol: %s\tValue: %s\n" s (dval2string l); printenv l'
;;

let rec printmem l v = match l with
    [] -> () |
    Nil::l' -> Printf.printf "\tnil\n"; printmem l' (v+1) |
    (Integer i)::l' -> Printf.printf "\t%d: %s\n" v (bigint2string i); printmem l' (v+1) |
    (Bool b)::l' -> (match b with
        true -> Printf.printf "\ttrue\n" |
        false -> Printf.printf "\tfalse\n"); printmem l' (v+1)
;;
  
let printdump (l1,l2) =
  Printf.printf "Env:\n"; printenv l1; 
  Printf.printf "Memory:\n"; printmem l2 0;;
  
(************************************************************)
(*                               TESTS                      *)
(************************************************************)

(* Repeat test *)
(*let testrepeat = Program(
  Dseq(Dseq(Const("b", Bool false),Var "x"),Var("a")),
  Cseq(Assign("a", False), Repeat(If (Or(Val("a"),Val("b")),Break,Assign("a",True)))))
;;
printdump (dump testrepeat);;*)


(* Or test *)
(*
let testor = Program(
  Dseq(Dseq(Const("b", Bool false),Var "c"),Var("a")),
  Cseq(Assign("a", False), Assign("c", Or(Val("a"),Val("b")))));;
printdump (dump testor);;
*)

(*let fact x = Program(
  Dseq(Const ("n", x),Dseq(Var "i",Var "f")),
  Cseq(Cseq(AssignVar ("i", N 1),AssignVar ("f", N 1)),
	   While(Or(Lt (Val "i", Val "n"),Eq (Val "i", Val "n")),
			 Cseq(AssignVar ("f", Mul (Val "f",Val "i")), 
				  AssignVar ("i", Add (Val "i",N 1))))))
;;

dump (fact 5);;*)


(*let test = Program(
  Dseq(Dseq(Const("b", Integer [-1;-2]),Var "x"),Var("a")),
  Assign("a",Add(Int [1;1;1],Val "b")))
;;
printdump (dump test);;*)

let fibonacci = Program(
  Dseq( Const("n",Integer[270]) , Dseq( VarInt "ris" , Dseq( VarInt "succ" , Dseq( VarInt "temp" , VarInt "i")))),
  Cseq( Assign("ris",Int [0]) , 
  Cseq( Assign("succ",Int [1]) , 
  Cseq( Assign("i",Int [0]) , 
  Repeat( 
    If( Eq( Val "i", Val "n") ,
      Break ,
      Cseq( Assign("temp", Val "succ") , 
      Cseq( Assign("succ", Add( Val "ris", Val "succ" )) , 
      Cseq( Assign("ris", Val "temp") , 
      Assign("i", Add( Val "i", Int [1] )))))))))));;
      
printdump (dump fibonacci);;
