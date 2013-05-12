(* Test file for bigintegers *)

(************************************************************)
(*                           BIGINTEGERS                    *)
(************************************************************)


(* bigint type *)
type bigint = int list;;

(*let num_max = 1000000000;;*)

let num_max = 100;;

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
type mval = Bool of bool | Integer of bigint | Nil;;

(*Printf.printf "%s" (bigint2string (bigint_add [-11;-05] [99]));; OK -1006*)

(*Printf.printf "%s" (bigint2string (bigint_add [13;05] [-11;-99]));; OK +106*)

(*Printf.printf "%s" (bigint2string (bigint_add [13;01] [-01]));; OK +1300*)

(*Printf.printf "%s" (bigint2string (bigint_add [1;00] [-9200]));; OK*)

(*Printf.printf "%s" (bigint2string (bigint_add [123555] [-454222]));; OK*)

(*Printf.printf "%s" (bigint2string (bigint_add [1;3] [-99]));; OK +4*)

(*Printf.printf "%s" (bigint2string (bigint_add [-99;-3] [-99;-99]));; OK -19902*)

(*Printf.printf "%s" (bigint2string (bigint_add [-1;-3] [-99]));; OK -202*)

(*Printf.printf "%s" (bigint2string (bigint_add [1;3] [99]));; OK 202*)

(*Printf.printf "%s" (bigint2string (bigint_add [-11;-3] [1;99]));; OK -904*)
