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
let emptystore = fun () -> Store ((fun x -> 0), 0x10000);;

exception AddressOutOfBounds of loc;;

let applystore (Store (sigma,dim)) l = 
    if 0 <= l && l < dim then sigma l
    else raise (AddressOutOfBounds l)
;;

let update (Store (sigma,dim)) l m = 
    if 0 <= l && l < dim then Store ((fun l' -> if l'=l then m else sigma l'), dim)
    else raise (AddressOutOfBounds l)
;;
