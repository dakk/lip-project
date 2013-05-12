type 'a set = 'a list;;

let rec member x = function
    [] -> false
  | y::l -> y=x || member x l;;

let rec mapflat f = function
  [] -> []
  | x::l -> f x @ mapflat f l;;

(* mapflat (fun x -> [x;x+1]) [1;3;5];; *)

let isempty l = (l=[]);;

let rec union l = function 
    [] -> l
  | x::l' -> if member x l then union l l' else x::(union l l');;

let rec intersect l = function
    [] -> []
  | x::l' -> if member x l then x::(intersect l l') else intersect l l';;

let rec setminus x = function
    [] -> []
  | y::l -> if x=y then setminus x l else y::(setminus x l);;

let rec set_of_list = function
    [] -> []
  | x::l -> if member x l then set_of_list l else x::(set_of_list l);;

let subseteq l l' = List.fold_right (fun x y -> if member x l' then y else false) l true;;

let equals l l' = subseteq l l' && subseteq l' l;;

(* type 'a partial = None | Some of 'a;; *)

let comp f g = fun x -> f (g x);;

let lub u v = match (u,v) with
  (_,None) -> u
| (None,_) -> v
| _ when u=v -> v
| _ -> failwith "Not deterministic"
;;

let join f g = fun x -> lub (f x) (g x);;

let rec loop p f x = if p x then x else loop p f (f x);;
let fixpoint f = loop (fun x -> equals (f x) x) f;;
let loop_or_fixpoint p f = loop (fun x -> p x || equals (f x) x) f;;
