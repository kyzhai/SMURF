open Ast
open Util

exception Multiple_declarations of string
exception Multiple_type_sigs of string
exception Type_mismatch of string
exception Main_wrong_scope

exception Type_error of string
let type_error msg = raise (Type_error msg)

type s_type = Int | Bool | Note | Beat | Chord | System | List of s_type |
              Poly of string | Unknown | Num

type s_ids = {
        name : string;
        v_type : s_type list;
}

type symbol_table = {
    parent : symbol_table option;
    identifiers : s_ids list; 
}

type s_func_decl = {
    s_fname : string; 
    type_sig : s_type list;
    s_args :  pattern list;
    s_value : expr;
    scope : symbol_table;
}

type s_dec = 
      STypesig of s_ids 
    | SFuncdec of s_func_decl
    | SVardef of s_ids * expr
    | SMain of expr 

type s_program = {
    decls : s_dec list;
    symtab : symbol_table;
}

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

let string_of_s_ids i = 
    "ID: " ^ i.name ^ " :: " ^ String.concat " -> "
    (List.map string_of_s_type i.v_type) ^ "\n"

let rec string_of_symbol_table symtab = 
    if symtab.parent = None then "Global Scope: \n\t" ^ 
        String.concat "\t" (List.map string_of_s_ids symtab.identifiers) ^ "\n"
    else (*(string_of_env p) ^ *)"\tNew Scope: " ^ 
        String.concat "\n\t" (List.map string_of_s_ids symtab.identifiers) ^"\n\t"

let string_of_s_func_decl f = 
        "Function: " ^ f.s_fname ^ " " ^ String.concat " " 
        (List.map Ast.string_of_patterns f.s_args) ^ " :: " ^ 
        String.concat " -> " (List.map string_of_s_type f.type_sig) ^ " =\n" 
        ^ Ast.string_of_expr f.s_value ^ "\n" ^ string_of_symbol_table f.scope

let string_of_s_dec = function
      STypesig(i) -> "STypesig: " ^ string_of_s_ids i
    | SFuncdec(f) -> "SFuncdec: " ^ string_of_s_func_decl f
    | SVardef(i, e) -> "SVardef: \n" ^ string_of_s_ids i ^ "\n\t" ^ Ast.string_of_expr e
    | SMain(e) -> "SMain: " ^ Ast.string_of_expr e

let string_of_s_program p = 
    "Program: " ^ String.concat "\n\t" 
    (List.map string_of_s_dec p.decls) ^ "\n" ^
    string_of_symbol_table p.symtab
