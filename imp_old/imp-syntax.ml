type ide = string;;

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

type dec =
    Empty
  | Const of ide * int
  | Var of ide
  | Array of ide * int
  | Dseq of dec * dec
;;

type com =
    Skip
  | AssignVar of ide * exp
  | AssignArray of ide * exp * exp
  | Cseq of com * com
  | If of exp * com * com
  | While of exp * com
;;

type prog = Program of dec * com;;

type idb = 
    Run of prog 
  | Echo of string
  | Quit
;;
