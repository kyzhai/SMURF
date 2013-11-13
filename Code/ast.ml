type operator = Add | Sub | Mul | Div | Mod | BeatDiv | BeatMul | BeatAdd | BeatSub | PCAdd | PCSub |
                BoolEq | And | Or | Less | Leq | Greater | Geq | BeatLess |
                BeatLeq | BeatGreater | BeatGeq | PCLess | PCLeq | PCGreater |
                PCGeq | Concat | Cons | Equal | Trans

type unary_operator = Not

type row_operator = Inv | Retro

(* Not sure if these should be here...doing it for type signature definition *)
type types = TInt | TBool | TNote | TBeat | TChord | TSystem | TList of types | TPoly of string | Unknown
    
type expr =                                 (* Expressions *)
      Literal of int                        (* 42 *)
    | Boolean of bool                       (* True *)
    | Variable of string                    (* bar *)
    | Beat of expr * int                    (* 2. *)
    | Note of  expr * expr * expr           (* (11, 2)^4. *)
    | Print of expr                         (* print 3+4 *)
    | Random                                (* random *)
    | Binop of expr * operator * expr       (* a + 2 *)
    | Unop of unary_operator * expr         (* ! a == 4 *)
    | Rowop of row_operator * expr          (* ~[1,2,3,4,5,6] *)
    | If of expr * expr * expr              (* if b == 4 then True else False *)
    | List of expr list                     (* [1,2,3,4] *)
    | Chord of expr list                    (* [Note1, Note2]*)
    | System of expr list                   (* [Chord1, Chord2]*)
    | Call of expr * expr                   (* foo a *)
    | Let of string * expr * expr           (* let x = 4 in x + 2 *)

type pattern =                              (* Patterns *)
    Patconst of int                         (* integer *)
    | Patbool of bool                       (* boolean *)
    | Patvar of string                      (* identifier*)
    | Patwild                               (* wildcard *)
    | Patcomma of pattern list              (* [pattern, pattern, pattern, ... ] or [] *)
    | Patcons of pattern * pattern          (* pattern : pattern *)

type func_decl = {                          (* Function Declaration *)
    fname : string;                         (* Function name *)
    args : pattern list;                    (* Pattern arguments *)
    value : expr;                           (* Expression bound to function *)
}

type dec =                                  (* Declarations *)
    Tysig of string * types list            (* f :: Int -> [Note] -> Bool *)
    | Funcdec of func_decl                  (* f x y = x + y *)
    | Vardef of string * expr               (* x = (2 + 5) : [1,2,3] *)
        | Main of expr                                                  (* main (f x) + (g x) *)

type program = dec list                     (* A program is a list of declarations *)

    (*
type stmt =                                 (* Statements *)
    Expr of expr                            (* c = 4 : [3,2,1] *)
    | If of expr * stmt * stmt              (* if b == 4 then True else False *)
    *)

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Boolean(b) -> string_of_bool b
  | Variable(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      ( match o with
        Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      | BoolEq -> "==" 
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=" 
      | Concat -> "++" | Cons -> ":" | m -> "OP" ) 
      ^ " " ^ string_of_expr e2
  | If(e1, e2, e3) -> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else " ^ string_of_expr e3
  | Beat(i1, i2) -> string_of_expr i1 ^ 
        let rec repeat n s = 
            if n>0 then 
                repeat (n-1) ("." ^ s)
            else s in repeat i2 ""
  | Note(pc, reg, bt) -> " (" ^ string_of_expr pc ^ ", " ^ string_of_expr reg ^ ")$" ^ (string_of_expr bt)
  | List(el) -> "[" ^ (String.concat ", " (List.map string_of_expr el)) ^ "]"
  | Chord(el) -> "[" ^ (String.concat ", " (List.map string_of_expr el)) ^ "]"
  | System(el) -> "[" ^ (String.concat ", " (List.map string_of_expr el)) ^ "]"
  | Let(decl, exp1, exp2) -> "let " ^ decl ^ " = " ^ string_of_expr exp1 ^ " in " ^ string_of_expr exp2
  | Call(exp1,exp2) -> string_of_expr exp1 ^ " " ^ string_of_expr exp2
  | x -> "other expr"

let rec string_of_patterns  = function
    Patconst(l) -> string_of_int l
    | Patbool(b) -> string_of_bool b
    | Patwild -> "_"
    | Patvar(s) -> s
    | Patcomma(p) -> "[" ^ (String.concat ", " (List.map string_of_patterns p)) ^ "]"
    | Patcons(p1, p2) -> (string_of_patterns p1) ^ " : " ^ (string_of_patterns p2)

let rec string_of_types  = function
    TInt -> "Int" | TBool -> "Bool" | TChord -> "Chord"
    | TNote -> "Note" | TBeat -> "Beat" | TSystem -> "System"
    | TList(t) -> "[" ^ string_of_types t ^ "]" | TPoly(v) -> "Poly " ^v    
        | Unknown -> "Type Unknown"

let string_of_dec  = function
    Tysig(id, types) -> id ^ " :: " ^ String.concat "-> " (List.map string_of_types types) ^ "\n"
  | Vardef(id, expr) -> id ^ " = " ^ string_of_expr expr ^ "\n"
  | Funcdec(fdec) -> fdec.fname ^ " " ^  String.concat " " (List.map string_of_patterns fdec.args) ^
    " = " ^ string_of_expr fdec.value ^ "\n"
    | Main(expr) -> "main " ^ string_of_expr expr ^ "\n"

let string_of_program decs =
  String.concat "" (List.map string_of_dec decs)
