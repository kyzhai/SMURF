open Ast

type env = {
		name : string;
		v_type : Ast.types list;
		parent : env;
	}

let first_pass list_decs = 
		(* Ast.string_of_program list_decs *)
		print_string "Look in sast.ml\n"; 0	
