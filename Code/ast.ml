type operator = Add | Sub | Mul | Div | Mod |
                Less | Leq | Greater | Geq |
                BeatAdd | BeatSub | BeatDiv | BeatMul |
                BeatLess | BeatLeq | BeatGreater | BeatGeq |
                PCAdd | PCSub |
                BoolEq | And | Or | Concat | Cons | Trans

type prefix_operator = Not | Inv | Retro

(* Not sure if these should be here...doing it for type signature definition *)
type types = TInt | TBool | TNote | TBeat | TChord | TSystem | TList of types |
              TPoly of string 

type expr =                                 (* Expressions *)
      Literal of int                        (* 42 *)
    | Boolean of bool                       (* True *)
    | Variable of string                    (* bar *)
    | Beat of expr * int                    (* 2. *)
    | Note of  expr * expr * expr           (* (11, 2)^4. *)
    | Binop of expr * operator * expr       (* a + 2 *)
    | Prefix of prefix_operator * expr      (* ! a == 4 *)
    | If of expr * expr * expr              (* if b == 4 then True else False *)
    | List of expr list                     (* [1,2,3,4] *)
    | Chord of expr list                    (* [(11,3)$4., (5,2)$4.]*)
    | System of expr list                   (* [ [(11,3)$4.,(5,2)$4.], [(-1,0)$2] ]*)
    | Call of string * fargs list           (* foo a *)
    | Let of dec list * expr                (* let x = 4 in x + 2 *)
		| Print of expr													(* print 3 *)

and dec =                                   (* Declarations *)
    Tysig of string * types list            (* f :: Int -> [Note] -> Bool *)
    | Funcdec of func_decl                  (* f x y = x + y *)
    | Vardef of string * expr               (* x = (2 + 5) : [1,2,3] *)
    | Main of expr                          (* main (f x) + (g x) *)

and func_decl = {                      (* Function Declaration *)
    fname : string;                         (* Function name *)
    args : pattern list;                    (* Pattern arguments *)
    value : expr;                           (* Expression bound to function *)
}

and pattern =                          (* Patterns *)
    Patconst of int                         (* integer *)
    | Patbool of bool                       (* boolean *)
    | Patvar of string                      (* identifier*)
    | Patwild                               (* wildcard *)
    | Patcomma of pattern list              (* [pattern, pattern, pattern, ... ] or [] *)
    | Patcons of pattern * pattern          (* pattern : pattern *)

and fargs =                                 (* Function Arguments *)
      Arglit of int                         (* 42 *)
    | Argbool of bool                       (* True *)
    | Argvar of string                      (* bar *)
    | Argbeat of expr * int                 (* 2. *)
    | Argnote of  expr * expr * expr        (* (11, 2)^4. *)
    | Argchord of expr list                 (* [(11,3)$4., (5,2)$4.] *)
    | Argsystem of expr list                (* [ [(11,3)$4.,(5,2)$4.], [(-1,0)$2] ] *)
    | Arglist of expr list                  (* [farg, farg, farg, ... ] or [] *)
    | Argparens of expr                     (* parenthesized expressions *)

type program = dec list                     (* A program is a list of declarations *)

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Boolean(b) -> string_of_bool b
  | Variable(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      ( match o with
        Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
      | BeatAdd -> "$+" | BeatSub -> "$-" | BeatMul -> "$*" | BeatDiv -> "$/"
      | PCAdd -> "%+" | PCSub -> "%-"
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      | BeatLess -> "$<" | BeatLeq -> "$<=" | BeatGreater -> "$>" | BeatGeq -> "$>="
      | And -> "&&" | Or -> "||" | BoolEq -> "=="
      | Concat -> "++" | Cons -> ":" | Trans -> "^^" )
      ^ " " ^ string_of_expr e2
  | Prefix(o, e) ->
      ( match o with Not -> "!" | Inv -> "~" | Retro -> "<>" )
      ^ " " ^ string_of_expr e
  | If(e1, e2, e3) -> "if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^
    " else " ^ string_of_expr e3
  | Beat(i1, i2) -> string_of_expr i1 ^ 
        let rec repeat n s = 
            if n>0 then 
                repeat (n-1) ("." ^ s)
            else s in repeat i2 ""
  | Note(pc, reg, bt) -> " (" ^ string_of_expr pc ^ ", " ^ string_of_expr reg ^ ")$" ^ (string_of_expr bt)
  | List(el) -> "[" ^ (String.concat ", " (List.map string_of_expr el)) ^ "]"
  | Chord(el) -> "[" ^ (String.concat ", " (List.map string_of_expr el)) ^ "]"
  | System(el) -> "[" ^ (String.concat ", " (List.map string_of_expr el)) ^ "]"
  | Call(fname,args) -> fname ^ " " ^ (String.concat " " (List.map string_of_args args))
  | Let(decl, exp) -> "let " ^ (String.concat " " (List.map string_of_dec decl)) ^ 
                      " in " ^ string_of_expr exp
	| Print(e) -> "print ("^(string_of_expr e)^")"

and string_of_fdec = function
    {fname;args;value} -> fname ^ " " ^  String.concat " " 
      (List.map string_of_patterns args) ^ " = " ^ string_of_expr value ^ "\n"

and string_of_dec  = function
    Tysig(id, types) -> id ^ " :: " ^ String.concat "-> " (List.map string_of_types types) ^
      "\n"
  | Vardef(id, expr) -> id ^ " = " ^ string_of_expr expr ^ "\n"
  | Funcdec(fdec) -> string_of_fdec fdec
  | Main(expr) -> "main " ^ string_of_expr expr ^ "\n"

and string_of_patterns  = function
    Patconst(l) -> string_of_int l
  | Patbool(b) -> string_of_bool b
  | Patwild -> "_"
  | Patvar(s) -> s
  | Patcomma(p) -> "[" ^ (String.concat ", " (List.map string_of_patterns p)) ^ "]"
  | Patcons(p1, p2) -> (string_of_patterns p1) ^ " : " ^ (string_of_patterns p2)

and string_of_types  = function
    TInt -> "Int" | TBool -> "Bool" | TChord -> "Chord"
  | TNote -> "Note" | TBeat -> "Beat" | TSystem -> "System"
  | TList(t) -> "[" ^ string_of_types t ^ "]" | TPoly(v) -> "Poly " ^ v

and string_of_args  = function
    Arglit(l) -> string_of_int l
  | Argbool(b) -> string_of_bool b
  | Argvar(s) -> s
  | Arglist(el) ->  "[" ^ (String.concat " " (List.map string_of_expr el)) ^ "]"
  | Argparens(p) -> "(" ^ (string_of_expr p)  ^ ")"
  | Argbeat(i1,i2) -> string_of_expr i1 ^
        let rec repeat n s =
            if n>0 then
                repeat (n-1) ("." ^ s)
            else s in repeat i2 ""
  | Argnote(pc, reg, bt) -> " (" ^ string_of_expr pc ^ ", " ^ string_of_expr reg ^ ")$" ^ (string_of_expr bt)
  | Argchord(el) -> "[" ^ (String.concat ", " (List.map string_of_expr el)) ^ "]"
  | Argsystem(el) -> "[" ^ (String.concat ", " (List.map string_of_expr el)) ^ "]"

let string_of_program decs =
  String.concat "" (List.map string_of_dec decs)

