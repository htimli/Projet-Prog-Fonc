type t =
  Int |Bool |Char 
  |Function of t * t
  |Pair of     t * t
;;

type constant =
  |Un |Deux |Trois |Quatre |Cinq
  |A  |B    |C     |D 
  |True     |False
;;      

type op_primitive =
  Plus             of t * t 
  |Moins           of t * t 
  |Inferieur       of t * t  
  |Superieur       of t * t 
  |If_then_else    of t * t * t
;;      

type exp =
    X of string
  |C of constant
  |Op of op_primitive 
  |Fun of string * t * exp
  |Application of exp * exp
  |Construction of exp * exp
  |Declaration of string * t * exp * exp
;;
(***************************************************)

let rec toString_of_type the_type =
  match the_type with
  |Int ->            "int"
  |Bool ->           "bool"
  |Char ->           "char"
  |Function(t1,t2)-> toString_of_type t1 ^"->"^ toString_of_type t2
  |Pair(t1,t2)->     toString_of_type t1 ^"*"^ toString_of_type t2
;;

let toString_of_const the_constant =
  match the_constant with
  |A ->      "a"
  |B ->      "b"
  |C ->      "c"
  |D ->      "d"
  |Un ->     "1"
  |Deux ->   "2"
  |Trois ->  "3"
  |Quatre -> "4"
  |Cinq ->   "5"
  |True ->   "true"
  |False ->  "false"
;;

let toString_of_operation the_operation =
  match the_operation with
  |Plus(t1,t2) -> " + "
  |Moins(t1,t2) -> " - "
  |Inferieur(t1,t2) -> " < "
  |Superieur(t1,t2) -> " > "
  |If_then_else(t1, t2, t3) -> " if_then_else " 
;;

let rec toString the_expression =
  match the_expression with
  |X(c) -> c
  |C(const) -> toString_of_const const
  |Op(op) -> toString_of_operation op
  |Fun(nom,t,exp)->"fun "^nom^" : "^toString_of_type t^" -> "^ toString exp
  |Application(exp1, exp2) -> toString exp1  ^ " " ^ toString exp2
  |Construction(exp1, exp2) -> "(" ^toString exp1 ^ ", " ^ toString exp2 ^ ")"
  |Declaration(name,t, exp1, exp2) -> " let " ^ name ^ " : " ^ toString_of_type t ^ " = " ^ toString exp1 ^ " in " ^ toString exp2
;;

(*exemple de la fonction successeur d'un entier*)
let v ="x";;
toString (Fun(v, Int, Application(Op(Plus(Int, Int)),Construction(X v, C(Un)))));;
