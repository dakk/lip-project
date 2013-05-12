open Camlp4.PreCast ;;

module ImpGram = MakeGram(Lexer) ;;

let exp  = ImpGram.Entry.mk "exp" ;;
let dec  = ImpGram.Entry.mk "dec" ;;
let com  = ImpGram.Entry.mk "com" ;;
let prog = ImpGram.Entry.mk "prog" ;;

EXTEND ImpGram

  exp: 
  [ 
    "equals" LEFTA
    [ e1 = exp; "="; e2 = exp -> Eq(e1,e2) 
    | e1 = exp; "<"; e2 = exp -> Lt(e1,e2) ]
  | "Sum,Sub" LEFTA
    [ e1 = exp; "+"; e2 = exp -> Add(e1,e2) 
    | e1 = exp; "-"; e2 = exp -> Sub(e1,e2) ]
  | "And,Or" LEFTA
    [ e1 = exp; "and"; e2 = exp -> And(e1,e2) 
    | e1 = exp; "or"; e2 = exp ->  Or(e1,e2) ]
  | "Mul" LEFTA
    [ e1 = exp; "*"; e2 = exp -> Mul(e1,e2) ]
  | "Not" RIGHTA
    [ "not"; e = exp -> Not(e) ]
  | "Values"
    [ `INT(i,_) -> N(i) 
    | `LIDENT x -> Val(x) 
    | `LIDENT x; "["; e = SELF; "]" -> ArrayVal(x,e) 
    | "("; e = SELF; ")" -> e ]
  ];

  dec:
  [
  [
    NEWLINE -> Empty
  | "const"; `LIDENT x; "="; `INT(v,_) -> Const(x,v)
  | "var"; `LIDENT x -> Var(x) 
  | "array"; `LIDENT x; "["; `INT(v,_); "]" -> Array(x,v) 		 
  | d1 = SELF; ";"; d2 = SELF -> Dseq(d1,d2) ]
  ];

  com:
  [
  [ 
    "skip" -> Skip
  | `LIDENT x; ":="; e = exp -> AssignVar(x,e)
  | `LIDENT x; "["; i = exp; "]"; ":="; e = exp -> AssignArray(x,i,e)
  | c1 = SELF; ";"; c2 = SELF -> Cseq(c1,c2)
  | "if"; e = exp; "then"; c1 = SELF; "else"; c2 = SELF -> If(e,c1,c2)
  | "while"; e = exp; "do"; c = SELF -> While(e,c)
  | "begin"; c = SELF; "end" -> c ]
  ];

  prog:
  [
  [
    "program"; c = com -> Program(Empty,c)
  | "program"; d = dec; c = com -> Program(d,c)
  ]
  ];

END ;;
