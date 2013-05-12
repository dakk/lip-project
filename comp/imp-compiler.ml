(* label index *)
let currentlabel = ref 0;;
let int_of_bool b = if b then 1 else 0;;

(* data types *)
type bigint = int list;;
type mval = Bool of bool | Integer of int | Bigint of bigint | Nil;;
type lval = LBool of int | LInteger of int | LNil;;


let rec rev l = 
  match l with
    [] -> []
  | x::xl -> (rev xl) @ [x]
;;

(************************************************************)
(*                         REGS HANDLING                    *)
(************************************************************)
let emptyreg () =
  let rec emptyreg' i = match i with
      64 -> [] 
    | _ -> (string_of_int i,false)::(emptyreg' (i+1))
  in (emptyreg' 2)
;;

let firstemptyreg r = 
  let rec firstemptyreg' r prev = match r with
      (i,true)::r' -> firstemptyreg' r' (prev@[(i,true)])
    | (i,false)::r' -> (i, (prev@[(i,true)]@r'))
    | [] -> ("-1", prev)
  in firstemptyreg' r []
;;

let rec unlockreg regs r  = match regs with
    (i,b)::regs' -> if i = r then (i,false)::regs' else (i,b)::(unlockreg regs' r)
  | [] -> []
;;



(************************************************************)
(*                       BIGINT COMPILATION                 *)
(************************************************************)
(* Memory representation:
    A Int variable point to a memory area that contains:
      - the size of the integer
      - the address of the first element
    Each Int element contains:
      - the value of the cell
      - the address of the next element
 *)

(* Bigint sum 
 * a1 -> address of the first operand
 * a2 -> address of the second operand
 * return -> assembly code 
 *)
let bigint_add a1 a2 f r =
  let max_num = "100" in
  let (rip,f1) = (firstemptyreg f) in
  let (sum,f2) = (firstemptyreg f1) in
  let (t1,f3) = (firstemptyreg f2) in
  let (t2,f4) = (firstemptyreg f3) in
  let (nmax,f5) = (firstemptyreg f4) in
  let (sl2,f6) = (firstemptyreg f5) in
  let (ncif,f7) = (firstemptyreg f6) in
  let loop = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
  let stop = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
  let stop2 = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
  let next = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
  [
    (* Init *)
    "addi $"^r^" 0 $sl";
    "addi $"^ncif^" 0 $0";
    "addi $"^a1^" 1 $"^a1;
    "addi $"^a2^" 1 $"^a2;
    "addi $"^rip^" 0 $0";
    "addi $"^nmax^" "^max_num^" $0";
    (* Loop *)
    loop^":";
    "load $"^a1^" $"^a1^" $0";
    "load $"^a2^" $"^a2^" $0";
    "beq $"^a1^" $"^a2^" "^stop;
    "load $"^t1^" $"^a1^" $0";
    "load $"^t2^" $"^a2^" $0";
    "add $"^sum^" $"^rip^" $"^t1;
    "add $"^sum^" $"^sum^" $"^t2;
    "slt $"^rip^" $"^nmax^" $"^sum;
    "beq $"^rip^" $0 "^next;
    "sub $"^sum^" $"^sum^" $"^nmax;
    next^":";
    "addi $sl 1 $sl";
    "addi $"^sl2^" 1 $sl";
    "store $sl $0 $"^sl2;
    "addi $sl 1 $sl";
    "store $sl $0 $"^sum;
    "addi $"^ncif^" 1 $"^ncif;
    "addi $"^a1^" 1 $"^a1;
    "addi $"^a2^" 1 $"^a2;
    "jmp "^loop;
    (* Stop *)
    stop^":";
    "beq $"^rip^" $0 "^stop2;
    "addi $sl 1 $sl";
    "addi $"^sl2^" 1 $sl";
    "store $sl $0 $"^sl2;
    "addi $sl 1 $sl";
    "store $sl $0 $"^rip;
    stop2^":";
    "addi $sl 1 $sl";    
    "store $sl $0 $0";
    "addi $sl 1 $sl";    
    "store $"^r^" $0 $"^ncif;
  ]
;;


let bigint_lt ra1 ra2 f r =
  let (r1,f1) = (firstemptyreg f) in
  let (r2,f2) = (firstemptyreg f1) in
  let (flag,f3) = (firstemptyreg f2) in
  let loop = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
  let cont = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
    ["addi $"^r^" 1 $0";
    loop^":";
    "load $"^r1^" $"^ra1^" $0";
    "load $"^r2^" $"^ra2^" $0";
    "slt $"^flag^" $"^r1^" $"^r2; 
    "bne $"^flag^" $0 "^cont;
    "addi $"^ra1^" 1 $"^ra1;
    "addi $"^ra2^" 1 $"^ra2;
    "load $"^ra1^" $"^ra1^" $0";
    "load $"^ra2^" $"^ra2^" $0";
    "bne $"^ra1^" $"^ra2^" "^loop;
    "addi $"^r^" 0 $0";
    cont^":"
    ]
;;



let bigint_eq ra1 ra2 f r =
  let (r1,f1) = (firstemptyreg f) in
  let (r2,f2) = (firstemptyreg f1) in
  let loop = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
  let cont = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
    ["addi $"^r^" 0 $0";
    loop^":";
    "load $"^r1^" $"^ra1^" $0";
    "load $"^r2^" $"^ra2^" $0";
    "bne $"^r1^" $"^r2^" "^cont;
    "addi $"^ra1^" 1 $"^ra1;
    "addi $"^ra2^" 1 $"^ra2;
    "load $"^ra1^" $"^ra1^" $0";
    "load $"^ra2^" $"^ra2^" $0";
    "bne $"^ra1^" $"^ra2^" "^loop;
    "addi $"^r^" 1 $0";
    cont^":"
    ]
;;

(* Bigint allocation 
 * i -> bigint of int list
 * l -> first empty location
 * return -> (new empty location, initialization asm code)
 *)
let bigint_alloc i l f =
  let rec bigint_alloc' i' l' f' asm = match i' with
      [x] -> let (r',f'') = (firstemptyreg f') in
              (l'+2, (asm@["addi $"^r'^" "^(string_of_int x)^" $0"; 
                     "store $sl $0 $"^r';
                     "addi $sl 1 $sl";
                     "store $sl $0 $0";
                     "addi $sl 1 $sl"]))
      | x::i'' -> (let (r',f'') = (firstemptyreg f') in
                  bigint_alloc' i'' (l'+2) f' (asm@["addi $"^r'^" "^(string_of_int x)^" $0"; 
                                       "store $sl $0 $"^r';
                                       "addi $sl 1 $sl";
                                       "addi $"^r'^" "^(string_of_int (l'+2))^" $0"; 
                                       "store $sl $0 $"^r';
                                       "addi $sl 1 $sl";]))
      | [] -> (l',asm)
  in bigint_alloc' i (l+2) f
        ( let (r',f') = (firstemptyreg f) in
          ["addi $"^r'^" "^(string_of_int (List.length i))^" $0"; 
           "store $sl $0 $"^r';
           "addi $sl 1 $sl";
           "addi $"^r'^" "^(string_of_int (l+2))^" $0"; 
           "store $sl $0 $"^r';
           "addi $sl 1 $sl";] )
;;



(* Bigint allocation 
 * i -> bigint of int list
 * get the first empty location from the $sl register
 * return -> initialization asm code
 *)
let bigint_alloc_exec i r f =
  let rec bigint_alloc' i' f' asm = match i' with
      [x] -> let (r',f'') = (firstemptyreg f') in
              (asm@["addi $"^r'^" "^(string_of_int x)^" $0"; 
                     "store $sl $0 $"^r';
                     "addi $sl 1 $sl";
                     "store $sl $0 $0";
                     "addi $sl 1 $sl"])
      | x::i'' -> (let (r',f'') = (firstemptyreg f') in
                  (bigint_alloc' i'' f' (asm@["addi $"^r'^" "^(string_of_int x)^" $0"; 
                                       "store $sl $0 $"^r';
                                       "addi $sl 1 $sl";
                                       "addi $"^r'^" 1 $sl"; 
                                       "store $sl $0 $"^r';
                                       "addi $sl 1 $sl"])))
      | [] -> asm
  in bigint_alloc' i f
        ( let (r',f') = (firstemptyreg f) in
          ["addi $"^r^" 0 $sl"; 
           "addi $"^r'^" "^(string_of_int (List.length i))^" $0"; 
           "store $sl $0 $"^r';
           "addi $sl 1 $sl";
           "addi $"^r'^" 1 $sl"; 
           "store $sl $0 $"^r';
           "addi $sl 1 $sl";] )
;;



(* Bigint copy
 * r1 -> reg that contains the address of the source
 * r -> reg where is stored the address of the destination
 * return -> initialization asm code
 *)
let bigint_copy r1 r f =
  let l = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
  let en = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
  let (r',f') = (firstemptyreg f) in
  let (r'',f'') = (firstemptyreg f') in
  [
    "load $"^r''^" $"^r1^" $0";
    "store $"^r^" $0 $"^r'';      
    "addi $"^r'^" 1 $"^r;
    "store $"^r'^" $0 $0";
    "addi $"^r1^" 1 $"^r1;
    "load $"^r1^" $"^r1^" $0";
    (* if the r1 size is not 0 set 'r' first pointer, else put zero and jmp end *)
    "beq $"^r''^" $0 "^en;
    "store $"^r'^" $0 $sl";
    l^":";
    "load $"^r'^" $"^r1^" $0";
    "store $sl $0 $"^r';
    "addi $sl 1 $sl";
    "addi $"^r'^" 1 $sl";
    "store $sl $0 $"^r';
    "addi $sl 1 $sl";
    "addi $"^r1^" 1 $"^r1;
    "load $"^r1^" $"^r1^" $0";
    "bne $"^r1^" $0 "^l;
    en^":";
    "addi $"^r1^" 1 $0";
    "sub $sl $sl $"^r1;
    "store $sl $0 $0";
    "addi $sl 1 $sl";
      
  ]
;;


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


(************************************************************)
(*                           TYPE CHECKING                  *)
(************************************************************)

type t = TInt | TBool | TErr;;

let rec typecheck rho = function
    Int i -> TInt
  | Add (e1,e2) -> 
    (match (typecheck rho e1,typecheck rho e2) with
      (TInt,TInt) -> TInt
      | _ -> TErr)
  | True -> TBool
  | False -> TBool
  | Not e -> 
      (match (typecheck rho e) with
        TBool -> TBool
      | _ -> TErr)
  | And (e1,e2) -> 
    (match (typecheck rho e1,typecheck rho e2) with
      (TBool,TBool) -> TBool
      | _ -> TErr)
  | Or (e1,e2) -> 
    (match (typecheck rho e1,typecheck rho e2) with
      (TBool,TBool) -> TBool
      | _ -> TErr)
  | Eq (i1,i2) -> 
    (match (typecheck rho i1,typecheck rho i2) with
      (TInt,TInt) -> TBool
      | _ -> TErr)
  | Lt (i1,i2) -> 
    (match (typecheck rho i1,typecheck rho i2) with
      (TInt,TInt) -> TBool
      | _ -> TErr)
  | Val x -> 
    (match applyenv rho x with
        DConst x' -> (match x' with
                          Bool b -> TBool
                        | Integer i -> TInt
                        | _ -> TErr )
      | DVar x' -> (match x' with
                          LBool b -> TBool
                        | LInteger i -> TInt
                        | _ -> TErr )
      | Unbound -> TErr);;

   

(*
    e: espressione
    r: registro destinazione
    rho: ambiente
    F: registri
    l: indice prossima etichetta libera 
 *)
let rec texp e r rho f = match e with
    True -> ["addi $" ^ r ^ " 1 $0"]
  | False -> ["addi $" ^ r ^ " 0 $0"]
  | Not e' -> if typecheck rho e <> TBool then failwith "Type error Not" else
      let (r',f') = (firstemptyreg f) in
      let cont = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
          (texp e' r' rho f') @ 
          ["addi $"^r^" 1 $0";
          "beq $"^r'^" $0 "^cont; 
          "addi $"^r^" 0 $0"; 
          cont^":"]
  | And (e1,e2) -> if typecheck rho e <> TBool then failwith "Type error And" else
      let (r1,f1) = (firstemptyreg f) in
      let (r2,f2) = (firstemptyreg f1) in
      let cont = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
          (texp e1 r1 rho f2) @ 
          (texp e2 r2 rho f2) @ 
          ["addi $"^r^" 0 $0"; 
          "beq $"^r1^" $0 "^cont; 
          "beq $"^r2^" $0 "^cont; 
          "addi $"^r^" 1 $0"; 
          cont^":"]
  | Or (e1,e2) -> if typecheck rho e <> TBool then failwith "Type error Or" else
      let (r1,f1) = (firstemptyreg f) in
      let (r2,f2) = (firstemptyreg f1) in
      let cont = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
          (texp e1 r1 rho f2) @ 
          (texp e2 r2 rho f2) @ 
          ["addi $"^r^" 1 $0"; 
          "bne $"^r1^" $0 "^cont; 
          "bne $"^r2^" $0 "^cont; 
          "addi $"^r^" 0 $0"; 
          cont^":"] 
  | Int i -> bigint_alloc_exec (rev i) r f
  | Add (e1,e2) -> if typecheck rho e <> TInt then failwith "Type error Add" else
      let (r1,f1) = (firstemptyreg f) in
      let (r2,f2) = (firstemptyreg f1) in
        (texp e1 r1 rho f2) @ 
        (texp e2 r2 rho f2) @
        bigint_add r1 r2 f2 r
  | Lt (e1,e2) -> if typecheck rho e <> TBool then failwith "Type error Lt" else
    let (r1,f1) = (firstemptyreg f) in
    let (r2,f2) = (firstemptyreg f1) in
      (texp e1 r1 rho f2) @ 
      (texp e2 r2 rho f2) @ 
      bigint_lt r1 r2 f2 r
  | Eq (e1,e2) -> if typecheck rho e <> TBool then failwith "Type error Eq" else
    let (r1,f1) = (firstemptyreg f) in
    let (r2,f2) = (firstemptyreg f1) in
      (texp e1 r1 rho f2) @ 
      (texp e2 r2 rho f2) @ 
      bigint_eq r1 r2 f2 r
  | Val i ->
    let l = applyenv rho i in
    let (r',f') = (firstemptyreg f) in
    (match l with
        DConst l' -> (match l' with
                          Bool b -> ["addi $"^r^" "^(string_of_int (int_of_bool b))^" $0"]
                        | Integer i -> ["addi $"^r^" "^(string_of_int i)^" $0"]
                        | _ -> failwith "Exception" )
      | DVar l' -> (match l' with
                            LBool b -> ["addi $"^r'^" "^(string_of_int b)^" $0";
                                        "load $"^r^" $"^r'^" $0"]
                          | LInteger i -> ["addi $"^r^" "^(string_of_int i)^" $0"]
                          | _ -> failwith "Exception")
      | Unbound -> failwith ("Unbound variable "^i))
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

let rec sem_dec d (r,f,l,asm) = match d with
  Empty -> (r,f,l,asm)
| Const(x,v) -> (match v with 
                    Bool b -> (bind r x (DConst v), f, l, asm)
                  | Bigint i -> 
                        let bd = (bigint_alloc (rev i) l f) in
                        (bind r x (DConst (Integer l)), f, fst bd, asm@(snd bd))
                  | _ -> failwith "Error")
| VarInt x -> (bind r x (DVar (LInteger l)), f, l+2,
                asm@["addi $sl "^(string_of_int l)^" $0";
                 "store $sl $0 $0";
                 "addi $sl "^(string_of_int (l+2))^" $0"])
| VarBool x -> ((bind r x (DVar (LBool l))), f, l+1, asm@["addi $sl 1 $sl"])
| Dseq(d1,d2) -> sem_dec d2 (sem_dec d1 (r,f,l,asm))
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


let rec tcom c rho f loopend = match c with
    Skip -> ["nop"]
  | Break -> if loopend<>"" then ["jmp "^loopend] else ["nop"]
  | Assign (i,e) -> 
    let l = applyenv rho i in
    let (r,f') = (firstemptyreg f) in
    let (r',f'') = (firstemptyreg f') in
    let (a,f''') = (firstemptyreg f'') in
    let loc = ( match l with 
                    DConst i -> failwith "Error" 
                  | Unbound -> failwith "Variable not exists"
                  | DVar i -> (match i with
                                  LBool b -> if typecheck rho e <> TBool then failwith "Type error loc bool" else string_of_int b
                                | LInteger i -> if typecheck rho e <> TInt then failwith "Type error loc int" else string_of_int i
                                | LNil -> failwith "Error") ) in    
      (match typecheck rho e with
          TBool -> (texp e r' rho f''')@
                      ["addi $"^a^" "^loc^" $0";
                       "store $"^a^" $0 $"^r']
        | TInt -> (texp e r' rho f''')@["addi $"^r^" "^loc^" $0"]@(bigint_copy r' r f''')
        | _ -> failwith "Error"
      )
  | Cseq (c1,c2) -> (tcom c1 rho f loopend) @ (tcom c2 rho f loopend)
  | If (e,c1,c2) -> if typecheck rho e <> TBool then failwith "Type error if" else
    let cont = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
    let ff = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
    let (r',f') = (firstemptyreg f) in
      (texp e r' rho f') @
      ["beq $"^r'^" $0 "^ff] @
      tcom c1 rho f' loopend @
      [ "jmp "^cont;
        ff^":" ] @
      tcom c2 rho f' loopend @
      [ cont^":" ]
  | Repeat c' -> 
    let lloop = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
    let cont = "L"^(string_of_int (currentlabel:=!currentlabel+1; !currentlabel)) in
      [lloop^":"] @
      tcom c' rho f cont @
      ["jmp "^lloop;
      cont^":"]
;;

(************************************************************)
(*                             PROGRAMS                     *)
(************************************************************)

type prog = Program of dec * com;;

let tprog (Program (d,c)) =
  let regs = emptyreg() in
  let (rho,f,l,cmd) = sem_dec d (emptyenv(),regs,2,[])
  in ["addi $sl 1 $0"]@cmd@(tcom c rho regs "")@["halt"]
;;

let isalabel s = (String.get s ((String.length s)-1))=':';;

let printlist ll = 
  let rec printlist' l pl = match l with
      [] -> ()
    | s::l' -> if isalabel s then
                  (if pl then 
                          (Printf.printf "nop\n%s " s; printlist' l' true)
                         else
                          (Printf.printf "%s " s; printlist' l' true))
               else
                  (Printf.printf "%s\n" s; printlist' l' false)
  in printlist' ll false
;;


let string_of_asm ll = 
  let rec string_of_asm' l pl = match l with
      [] -> ""
    | s::l' -> if isalabel s then
                  (if pl then 
                          ("nop\n"^s^" "^(string_of_asm' l' true))
                         else
                          (s^" "^(string_of_asm' l' true)))
               else
                  (s^"\n"^(string_of_asm' l' false))
  in string_of_asm' ll false
;;

let string_to_file f s =
  let ch = open_out f in
  output_string ch s;
  close_out ch
;;

let compile f p = string_to_file f (string_of_asm (["DEBUG MEM 0 160"]@(tprog p)));;

(*
printlist (texp (And ((Not (Not True)),(Or (False,(Not True))))) "9" (emptyenv ()) (emptyreg ()));;
*)



(*compile "test1.asm" (Program(
  Dseq(
    Const("n1",Integer 1),
    Dseq( Const("n2",Integer 10),
    VarInt "v")
  ),
  Repeat(
    If( Eq( Val "v", Int 7),
      Break,
      Assign("v", Add( Val "v", Int 1))
    )
  )
));;*)
(*
compile "test1.asm" (Program(
  Dseq(
    Const("n1",Integer 1),
    Dseq( Const("n2",Integer 10),
    VarInt "v")
  ),
  Repeat(
    If( Eq( Val "v", Int 7),
      Break,
      Assign("v", Add( Val "v", True))
    )
  )
));;*)


(*compile "../asm/test1.asm" (Program(
  Dseq(
    Const ("x1", Bigint[1;3;3]),
  Dseq(
    Const ("x2", Bigint[1;3]),
    VarBool "r")),
    Assign ("r",Lt (Val "x1", Val "x2" )) ));;
(*printlist (snd (bigint_alloc [1;2;3] 1 (emptyreg ())));*)

compile "../asm/test1.asm" (Program(
    VarBool "r",
    Assign ("r",Lt (Int [30;31;32], Int [40;41;42] ))));;*)

(*    
compile "../asm/test1.asm" (Program(
    VarInt "r",
    Assign ("r", Add(Int [99;99], Int [37]))));;
*)
(*string_to_file "../asm/test1.asm" (string_of_asm (texp (Add(Int [1], Int [47])) "s1" (emptyenv ()) (emptyreg ())));*)



(* 
	Fibonacci program in Imp
		M[2] -> Size of the result
		M[3] -> Pointer of the first result's element 
*)
compile "fib.asm" (Program(
  Dseq( VarInt "ris" , Dseq( Const("n",Bigint [6]) , Dseq( VarInt "succ" , Dseq( VarInt "temp" , VarInt "i")))),
  Cseq( Assign("ris",Int [0]) , 
  Cseq( Assign("succ",Int [1]) , 
  Cseq( Assign("i",Int [0]) , 
  Repeat( 
    If( Eq( Val "i", Val "n") ,
      Break ,
      Cseq( Assign("temp", Val "succ") , 
      Cseq( Assign("succ", Add( Val "ris", Val "succ" )) , 
      Cseq( Assign("ris", Val "temp") , 
      Assign("i", Add( Val "i", Int [1] )))))))))))
);;
