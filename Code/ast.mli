type operator = Add | Sub | Mul | Div | Mod | BeatAdd | BeatSub | PCAdd | PCSub |
                BoolEq | Not | And | Or | Less | Leq | Greater | Geq | BeatLess | 
                BeatLeq | BeatGreater | BeatGeq | PCLess | PCLeq | PCGreater | 
                PCGeq | Concat | Cons | Equal | TypeSpec | ArgTypeSpec

type row_operator = Inv | Retro | Trans

type expr =                               (* Expressions *)
  Literal of int                          (* 42 *)
  | Variable of string                    (* bar *)
  | Binop of expr * operator * expr       (* a + 2 *)
  | Rowop of row_operator * expr          (* ~[1,2,3,4,5,6] *)
  | Assign of string * expr               (* b = 4 *)

type stmt =                               (* Statements *)
  Expr of expr                            (* c = 4 : [3,2,1] *)
  | If of expr * stmt * stmt              (* if b == 4 then True else False *)

