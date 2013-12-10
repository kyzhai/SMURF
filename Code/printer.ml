open Ast
open Sast
open Values

let print_ast_expr = function
    _ as e -> print_string (Ast.string_of_expr e)

let print_ast_fdec = function
    _ as e -> print_string (Ast.string_of_fdec e)

let print_ast_dec = function
    _ as e -> print_string (Ast.string_of_dec e)

let print_ast_patterns = function
    _ as e -> print_string (Ast.string_of_patterns e)
    
let print_ast_types = function
    _ as e -> print_string (Ast.string_of_types e)

let print_ast_args = function
    _ as e -> print_string (Ast.string_of_args e)
    
let print_ast_program = function
    _ as e -> print_string (Ast.string_of_program e)

let print_sast_sexpr = function
    _ as e -> print_string (Sast.string_of_sexpr e)

let print_sast_sfargs = function
    _ as e -> print_string (Sast.string_of_sfargs e)

let print_sast_s_dec = function
    _ as e -> print_string (Sast.string_of_s_dec e)

let print_sast_s_ids = function
    _ as e -> print_string (Sast.string_of_s_ids e)

let print_sast_s_func_decl = function
    _ as e -> print_string (Sast.string_of_s_func_decl e)

let print_sast_s_type = function
    _ as e -> print_string (Sast.string_of_s_type e)

let print_sast_symbol_table = function
    _ as e -> print_string (Sast.string_of_symbol_table e)

let print_sast_s_program = function
    _ as e -> print_string (Sast.string_of_s_program e)

let print_values_value = function
    _ as e -> print_string (Values.string_of_value e)



