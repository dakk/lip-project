(************************************************************)
(*                           ENVIRONMENT                    *)
(************************************************************)

(* env: function from identifiers (ide) to dval *)

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

