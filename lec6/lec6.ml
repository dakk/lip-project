(************************************************************)
(*                           ENVIRONMENT                    *)
(************************************************************)

(* env: function from identifiers (ide) to dval *)

type ide = string;;
type loc = int;;

(* dval: semantic domain of environments *)

type dval =
  Unbound
| DConst of int
| DVar of loc
| DArray of loc
;;

type env = Env of (ide -> dval)

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

type mval = int;;

(* store: pair (s,d), where 
          s is function from locations (loc) to mval
          d is an integer (size of the store, set to 2^16)
*)

type store = Store of (loc -> mval) * int;;

(* interface with the store *)
let emptystore = fun () -> Store ((fun x -> 0), 0x1FFFF);;

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
  N of int
| Val of ide
| ArrayVal of ide * exp
| Add of exp * exp
| Sub of exp * exp
| Mul of exp * exp
| Eq of exp * exp
| Lt of exp * exp
| Not of exp
| And of exp * exp
| Or of exp * exp
;;

exception TypeMismatch of string;;
 
let rec sem_exp e (r, s) = match e with
  N n -> n
| Val x ->  
    (match applyenv r x with
      DVar l -> applystore s l
    | DConst c -> c
    | DArray l -> raise (TypeMismatch (
        "You have tried to access array" ^ x ^ " as a variable"))
    | _ -> raise (UnboundIde x)
    )
| ArrayVal (x,e') ->  
    (match applyenv r x with
      DArray l -> applystore s (l + sem_exp e' (r,s))
    | DVar l -> raise (TypeMismatch (
        "You have tried to access variable " ^ x ^ " as an array"))
    | DConst c -> raise (TypeMismatch (
        "You have tried to access constant " ^ x ^ " as an array"))
    | Unbound -> raise (UnboundIde x)
    )
| Add (e1,e2) -> sem_exp e1 (r,s) + sem_exp e2 (r,s)
| Sub (e1,e2) -> sem_exp e1 (r,s) - sem_exp e2 (r,s)
| Mul (e1,e2) -> sem_exp e1 (r,s) * sem_exp e2 (r,s)
| Eq (e1,e2) -> if sem_exp e1 (r,s) = sem_exp e2 (r,s) then 1 else 0
| Lt (e1,e2) -> if sem_exp e1 (r,s) < sem_exp e2 (r,s) then 1 else 0
| Not e' -> if sem_exp e' (r,s) = 0 then 1 else 0
| And (e1,e2) -> if sem_exp e1 (r,s) = 0 || sem_exp e2 (r,s) = 0 then 0 else 1
| Or (e1,e2) -> if sem_exp e1 (r,s) = 0 && sem_exp e2 (r,s) = 0 then 0 else 1
;;


(************************************************************)
(*                           DECLARATIONS                   *)
(************************************************************)

(* dec: IMP declarations *)

type dec =
    Empty
  | Const of ide * int
  | Var of ide
  | Array of ide * int
  | Dseq of dec * dec
;;

let rec sem_dec d (r,l) = match d with
  Empty -> (r,l)
| Const(x,v) -> (bind r x (DConst v), l)
| Var x -> (bind r x (DVar l), l+1)
| Array (x,v) -> (bind r x (DArray l), l+v)
| Dseq(d1,d2) -> sem_dec d2 (sem_dec d1 (r,l))
;;


(************************************************************)
(*                             COMMANDS                     *)
(************************************************************)

(* com: IMP commands *)

type com =
    Skip
  | AssignVar of ide * exp
  | AssignArray of ide * exp * exp
  | Cseq of com * com
  | If of exp * com * com
  | While of exp * com
;;

let rec sem_com c (rho,sigma) = match c with
    Skip -> sigma
  | AssignVar(x,e) -> (match applyenv rho x with
      DVar l -> update sigma l (sem_exp e (rho,sigma))
    | _ -> raise (TypeMismatch ("You have tried to assign to non-variable" ^ x)))
  | AssignArray(x,e,e') -> (match applyenv rho x with
      DArray l -> update sigma (l + sem_exp e (rho,sigma)) (sem_exp e' (rho,sigma))
    | _ -> raise (TypeMismatch ("You have tried to assign to non-array" ^ x)))
  | Cseq(c1,c2) -> 
      sem_com c2 (rho,sem_com c1 (rho,sigma))
  | If(e,c1,c2) -> 
      if sem_exp e (rho,sigma) = 0 
      then sem_com c2 (rho,sigma) 
      else sem_com c1 (rho,sigma)
  | While(e,c') -> 
      if sem_exp e (rho,sigma) = 0 
      then sigma 
      else sem_com c (rho,sem_com c' (rho,sigma))
;;


(************************************************************)
(*                             PROGRAMS                     *)
(************************************************************)

type prog = Program of dec * com;;

let sem_prog (Program (d,c)) =
  let (rho,l) = sem_dec d (emptyenv(),0)
  in sem_com c (rho,emptystore())
;;

(* functions used for debugging programs *)

let rec range a b = 
  if a>b then []
  else a::(range (a+1) b);;
 
let rec ide = function
    Empty -> []
  | Const (x,v) -> [x]
  | Var x -> [x]
  | Array (x,v) -> [x]
  | Dseq (d1,d2) -> (ide d1) @ (ide d2)
;;
 
let dump (Program (d,c)) = 
  let (r,l) = sem_dec d (emptyenv(),0)
  in let s' = sem_com c (r,emptystore())
  in (List.map (fun x -> (x,applyenv r x)) (ide d), 
      List.map (applystore s') (range 0 (l-1)))
;;

(************************************************************)
(*                               TESTS                      *)
(************************************************************)

let test1 = Program(
  Dseq(Const("n", 5),Var "x"),
  If(Lt(Val "n",N 7),AssignVar ("x", Sub(Val "n",N 1)),AssignVar ("x", Add(Val "n",N 1))))
;;

dump test1;;

let fact x = Program(
  Dseq(Const ("n", x),Dseq(Var "i",Var "f")),
  Cseq(Cseq(AssignVar ("i", N 1),AssignVar ("f", N 1)),
	   While(Or(Lt (Val "i", Val "n"),Eq (Val "i", Val "n")),
			 Cseq(AssignVar ("f", Mul (Val "f",Val "i")), 
				  AssignVar ("i", Add (Val "i",N 1))))))
;;

dump (fact 5);;
