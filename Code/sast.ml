open Ast
open Util

exception Multiple_declarations of string
exception Multiple_type_sigs of string
exception Multiple_patterns of string 
exception Type_mismatch of string
exception Main_wrong_scope

exception Type_error of string
let type_error msg = raise (Type_error msg)

type s_type = Int | Bool | Note | Beat | Chord | System | List of s_type |
              Poly of string | Unknown | Num | Still_unknown

type s_ids = {
        name : string;
        v_type : s_type list;
        v_expr : expr option;
}

type symbol_table = {
    parent : symbol_table option;
    identifiers : s_ids list; 
}



type s_program = {
    decls : s_dec list;
    symtab : symbol_table;
}

and s_expr =
      SLiteral of int                        (* 42 *)
    | SBoolean of bool                       (* True *)
    | SVariable of string                    (* bar *)
    | SBeat of s_expr * int                    (* 2. *)
    | SNote of  s_expr * s_expr * s_expr           (* (11, 2)^4. *)
    | SBinop of s_expr * operator * s_expr       (* a + 2 *)
    | SPrefix of prefix_operator * s_expr      (* ! a == 4 *)
    | SIf of s_expr * s_expr * s_expr              (* if b == 4 then True else False *)
    | SList of s_expr list                     (* [1,2,3,4] *)
    | SChord of s_expr list                    (* [(11,3)$4., (5,2)$4.]*)
    | SSystem of s_expr list                   (* [ [(11,3)$4.,(5,2)$4.], [(-1,0)$2] ]*)
    | SCall of s_expr * s_expr                   (* foo a *)
    | SLet of s_expr * s_program               (* let x = 4 in x + 2 *)

and s_dec = 
      STypesig of s_ids 
    | SFuncdec of s_func_decl
    | SVardef of s_ids * s_expr
    | SMain of s_expr 

and  s_func_decl = {
    s_fname : string; 
    type_sig : s_type list;
    s_args :  pattern list;
    s_value : s_expr;
    scope : symbol_table;
}


let rec string_of_sexpr = function
    SLiteral(l) -> string_of_int l
  | SBoolean(b) -> string_of_bool b
  | SVariable(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^
      ( match o with
        Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
      | BeatAdd -> "$+" | BeatSub -> "$-" | BeatMul -> "$*" | BeatDiv -> "$/"
      | PCAdd -> "%+" | PCSub -> "%-"
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      | BeatLess -> "$<" | BeatLeq -> "$<=" | BeatGreater -> "$>" | BeatGeq -> "$>="
      | And -> "&&" | Or -> "||" | BoolEq -> "=="
      | Concat -> "++" | Cons -> ":" | Trans -> "^^" )
      ^ " " ^ string_of_sexpr e2
  | SPrefix(o, e) ->
      ( match o with Not -> "!" | Inv -> "~" | Retro -> "<>" )
      ^ " " ^ string_of_sexpr e
  | SIf(e1, e2, e3) -> "if " ^ string_of_sexpr e1 ^ " then " ^ string_of_sexpr e2 ^
    " else " ^ string_of_sexpr e3
  | SBeat(i1, i2) -> string_of_sexpr i1 ^ 
        let rec repeat n s = 
            if n>0 then 
                repeat (n-1) ("." ^ s)
            else s in repeat i2 ""
  | SNote(pc, reg, bt) -> " (" ^ string_of_sexpr pc ^ ", " ^ string_of_sexpr reg ^ ")$" ^ (string_of_sexpr bt)
  | SList(el) -> "[" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "]"
  | SChord(el) -> "[" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "]"
  | SSystem(el) -> "[" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "]"
  | SCall(exp1,exp2) -> string_of_sexpr exp1 ^ " " ^ string_of_sexpr exp2
  | SLet(exp, prog) -> "let " ^ " NEED TO FIX THIS PART " ^ 
                      " in " ^ string_of_sexpr exp

let rec string_of_s_type = function
      Int -> "Int"
    | Bool -> "Bool"
    | Note -> "Note"
    | Beat -> "Beat"
    | Chord -> "Chord"
    | System -> "System"
    | List(t) -> "[" ^ string_of_s_type t ^ "]"
    | Poly(s) -> s
    | Unknown -> "Unknown"
    | Num -> "Num"
		| Still_unknown -> "Still Unknown"

let string_of_s_ids i = 
    "ID: " ^ i.name ^ " :: " ^ String.concat " -> "
    (List.map string_of_s_type i.v_type) ^ "\n"

let rec string_of_symbol_table symtab = 
    if symtab.parent = None then "Global Scope: \n\t" ^ 
        String.concat "\t" (List.map string_of_s_ids symtab.identifiers) ^ "\n"
    else (*(string_of_env p) ^ *)"\tNew Scope: \n\t\t" ^ 
        String.concat "\t\t" (List.map string_of_s_ids symtab.identifiers) ^"\n\t"

let string_of_s_func_decl f = 
        f.s_fname ^ " " ^ String.concat " " 
        (List.map Ast.string_of_patterns f.s_args) ^ " :: " ^ 
        String.concat " -> " (List.map string_of_s_type f.type_sig) ^ " = " 
        ^ string_of_sexpr f.s_value ^ "\n" ^ string_of_symbol_table f.scope

let string_of_s_dec = function
      STypesig(i) -> "STypesig: \n\t\t" ^ string_of_s_ids i
    | SFuncdec(f) -> "SFuncdec: \n\t\t" ^ string_of_s_func_decl f
    | SVardef(i, e) -> "SVardef: \n\t\t" ^ string_of_s_ids i ^ "\n\t" ^ string_of_sexpr e
    | SMain(e) -> "SMain: " ^ string_of_sexpr e

let string_of_s_program p = 
    "Program: \n\t" ^ String.concat "\n\t" 
    (List.map string_of_s_dec p.decls) ^ "\n" ^
    string_of_symbol_table p.symtab
