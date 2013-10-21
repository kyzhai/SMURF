type operator = Add | Sub | Mul | Div | Mod | BeatDiv | BeatMul | BeatAdd | BeatSub | PCAdd | PCSub |
                BoolEq | And | Or | Less | Leq | Greater | Geq | BeatLess | 
                BeatLeq | BeatGreater | BeatGeq | PCLess | PCLeq | PCGreater | 
                PCGeq | Concat | Cons | Equal | TypeSpec | ArgTypeSpec

type unary_operator = Not

type row_operator = Inv | Retro | Trans

type types = Int | Bool | Note | Beat | Chord | System | [types]   (* Not sure if these should be here...doing it 
                                                                         for type signature definition *)

type expr =                                 (* Expressions *)
    Literal of int                          (* 42 *)
    | Variable of string                    (* bar *)
    | Binop of expr * operator * expr       (* a + 2 *)
    | Unop of unary_operator * expr         (* ! a == 4 *)
    | Rowop of row_operator * expr          (* ~[1,2,3,4,5,6] *)

type dec =                                  (* Declarations *)
    Tysig of string * types list            (* f :: Int -> [Note] -> Bool *)
    Funcdec of func_decl list               (* f x y = x + y *)

type func_decl = {                          (* Function Declaration *)
    fname : string;                         (* Function name *)
    args : pattern list;                    (* Pattern arguments *)
    value : expr;                           (* Expression bound to function *)
}

type pattern =                              (* Patterns *)
    Patconst of int                         (* integer or boolean constant *)
    Patvar of string                        (* identifier *)
    Patlist of pattern list                 (* x:rest or [1,2,3] *)

type def =                                  (* Definitions *)
    Vardef of string * expr                 (* x = (2 + 5) : [1,2,3] *)

type stmt =                                 (* Statements *)
    Expr of expr                            (* c = 4 : [3,2,1] *)
    | If of expr * stmt * stmt              (* if b == 4 then True else False *)

