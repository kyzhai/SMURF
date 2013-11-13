open Ast
open Util

exception Multiple_declarations of string
exception Multiple_type_sigs of string
exception Type_mismatch of string
exception Main_wrong_scope

exception Type_error of string
let type_error msg = raise (Type_error msg)

type s_ids = {
	name : string;
	v_type : types list;
}
	
type symbol_table = {
    parent : symbol_table option;
    identifiers : s_ids list; 
}

type s_func_decl = {
    s_fname : string; 
    type_sig : types list;
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

let string_of_s_ids i = 
    "ID: " ^ i.name ^ " :: " ^ String.concat " -> "
    (List.map Ast.string_of_types i.v_type) ^ "\n"


let rec string_of_symbol_table symtab = 
    if symtab.parent = None then "Global Scope: \n\t" ^ 
        String.concat "\t" (List.map string_of_s_ids symtab.identifiers) ^ "\n"
    else (*(string_of_env p) ^ *)"\tNew Scope: " ^ 
        String.concat "\n\t" (List.map string_of_s_ids symtab.identifiers) ^"\n\t"




