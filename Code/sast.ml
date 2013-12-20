open Ast
open Util

exception Multiple_declarations of string
exception Multiple_type_sigs of string
exception Multiple_patterns of string 
exception Pattern_list_type_mismatch of string 
exception Cons_pattern_type_mismatch of string
exception Multiple_identical_pattern_lists of string
exception No_type_signature_found of string
exception No_func_dec of string
exception Pattern_num_mismatch of int * int
exception Type_mismatch of string
exception Main_wrong_scope
exception Main_type_mismatch of string
exception Main_missing 
exception Function_used_as_variable of string
exception Missing_variable_definition of string
exception Function_not_defined of string
exception Wrong_number_of_arguments of string
exception Function_arguments_type_mismatch of string
exception Type_error of string
let type_error msg = raise (Type_error msg)

type s_type = Int | Bool | Note | Beat | Chord | System | List of s_type |
              Poly of string | Unknown | Num | Still_unknown | Empty
type s_program = {
    mutable decls : s_dec list;
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
    | SCall of string * s_arg list             (* foo a b *)
    | SLet of s_program * s_expr               (* let x = 4 in x + 2 *)
    | SRandom
    | SPrint of s_expr

and s_arg =
    SArglit of int                             (* integer *)
    | SArgbool of bool                         (* boolean *)
    | SArgvar of string                        (* identifiers *)
    | SArgbeat of s_expr * int                 (* 2. *)
    | SArgnote of  s_expr * s_expr * s_expr    (* (11, 2)^4. *)
    | SArgchord of s_expr list                 (* [(11,3)$4., (5,2)$4.] *)
    | SArgsystem of s_expr list                (* [ [(11,3)$4.,(5,2)$4.], [(-1,0)$2] ] *)
    | SArglist of s_expr list                  (* expression *)
    | SArgparens of s_expr                     (* parenthesized expressions *)

and s_dec = 
     SFuncdec of s_func_decl
    | SVardef of s_ids * s_expr
    | SMain of s_expr 

and  s_func_decl = {
    s_fname : string; 
    type_sig : s_type list;
    s_args :  pattern list;
    s_value : s_expr;
    scope : symbol_table;
}


and s_ids = {
        name : string;
        pats : pattern list;
        v_type : s_type list;
        v_expr : s_expr option;
}

and symbol_table = {
    parent : symbol_table option;
    mutable identifiers :  s_ids list; 
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
  | SCall(fname,args) -> fname ^ " " ^ (String.concat " " (List.map string_of_sfargs args))
  | SLet(decs, exp) -> "let " ^ (String.concat " " (List.map string_of_s_dec decs.decls)) ^ 
                      " in " ^ string_of_sexpr exp
  | SRandom -> "random"
  | SPrint(e) -> "print " ^ string_of_sexpr e

and string_of_sfargs = function
    SArglit(l) -> string_of_int l
  | SArgbool(b) -> string_of_bool b
  | SArgvar(s) -> s
  | SArgbeat(i1, i2) -> string_of_sexpr i1 ^
        let rec repeat n s =
            if n>0 then
                repeat (n-1) ("." ^ s)
            else s in repeat i2 ""
  | SArgnote(pc, reg, bt) -> " (" ^ string_of_sexpr pc ^ ", " ^ string_of_sexpr reg ^ ")$" ^ (string_of_sexpr bt)
  | SArgchord(el) -> "[" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "]"
  | SArgsystem(el) -> "[" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "]"

  | SArglist(el) -> "[" ^ (String.concat ", " (List.map string_of_sexpr el)) ^ "]"
  | SArgparens(p) -> "(" ^ (string_of_sexpr p)  ^ ")"

and string_of_s_dec = function
    | SFuncdec(f) -> "SFuncdec: \n\t\t" ^ string_of_s_func_decl f
    | SVardef(i, e) -> "SVardef: \n\t\t" ^ string_of_s_ids i ^ "\n\t" ^ string_of_sexpr e
    | SMain(e) -> "SMain: " ^ string_of_sexpr e

and string_of_s_ids i = 
    let str = if (i.pats <> []) then String.concat " " (List.map string_of_patterns i.pats) 
              else "" in
    "ID: " ^ i.name ^ " " ^ str ^ " :: " ^ String.concat " -> "
    (List.map string_of_s_type i.v_type) ^ "\n" 

and string_of_s_func_decl f = 
        f.s_fname ^ " (" ^ String.concat ") (" 
        (List.map Ast.string_of_patterns f.s_args) ^ ") :: " ^ 
        String.concat " -> " (List.map string_of_s_type f.type_sig) ^ " = " 
        ^ string_of_sexpr f.s_value ^ "\n" ^ string_of_symbol_table f.scope

and string_of_s_type = function
      Int -> "Int"
    | Bool -> "Bool"
    | Note -> "Note"
    | Beat -> "Beat"
    | Chord -> "Chord"
    | System -> "System"
    | List(t) -> "[" ^ string_of_s_type t ^ "]"
    | Empty -> "[]"
    | Poly(s) -> s
    | Unknown -> "Unknown"
    | Num -> "Num"
		| Still_unknown -> "Still Unknown"

and string_of_symbol_table symtab = 
    if symtab.parent = None then "Global Scope: \n\t" ^ 
        String.concat "\t" (List.map string_of_s_ids symtab.identifiers) ^ "\n"
    else (*(string_of_env p) ^ *)"\tNew Scope: \n\t\t" ^ 
        String.concat "\t\t" (List.map string_of_s_ids symtab.identifiers) ^"\n\t"


let string_of_s_arg = function 
	SArglit(i) -> string_of_int i
	| SArgbool(b) -> string_of_bool b
	| SArgvar(s) -> s
	| SArgbeat(e,i) -> (string_of_sexpr e) ^"^"^string_of_int i
	| SArgnote(e1,e2,e3) -> "("^(string_of_sexpr e1)^","^(string_of_sexpr e2)^")$"^(string_of_sexpr e3)
	| SArgchord(el) -> (string_of_sexpr (SChord(el)))
	| SArgsystem(el) -> (string_of_sexpr (SSystem(el)))
	| SArglist(el) -> (string_of_sexpr (SList(el)))
	| SArgparens(e) -> (string_of_sexpr e)

let string_of_s_program p = 
    "Program: \n\t" ^ String.concat "\n\t" 
    (List.map string_of_s_dec p.decls) ^ "\n" ^
    string_of_symbol_table p.symtab
