open Ast

exception Multiple_declarations of string
exception Main_wrong_scope

type var = {
		name : string;
		v_type : Ast.types list;
	}

type env =
	Parent of var list
	| Child of var list * env

let string_of_var v = 
	"Var: " ^ v.name ^ " :: " ^ String.concat " -> " (List.map Ast.string_of_types v.v_type) ^ "\n"


let rec string_of_env = function
	Parent(v) -> "Global Scope: \n\t" ^ 
		String.concat "\t" (List.map string_of_var v) ^ "\n\t"
	| Child(v,p) -> (string_of_env p) ^ "\tNew Scope: " ^ 
		String.concat "\n\t" (List.map string_of_var v) ^"\n\t"

let rec in_list x = function
	[] -> false
	| h::tl -> if x = h.name then true else in_list x tl

let rec is_declared id = function
	Parent(l) -> if (in_list id l) then true else false
	| Child(l, p) -> if (in_list id l) then true else is_declared id p 

let add_var v = function
	Parent(l) -> Parent(v :: l)
	| Child(l, p) -> Child(v :: l, p)


let global_env = Parent([])

let walk_decl scope = function
		Tysig(id,types) -> print_string "type signature\n"; 
				let func = {name=id; v_type = types} in 
				if (is_declared id scope) then raise (Multiple_declarations id)
				else add_var func scope 
	| Vardef(id, expr) -> print_string "var definition\n"; 
				if( is_declared id scope) then raise (Multiple_declarations id)
				else add_var {name=id; v_type = [Unknown]} scope
	| Funcdec(fdec) -> print_string "function declaration\n"; scope
	| Main(expr) -> print_string "main\n"; 
		match scope with 
			Parent(l) -> if( is_declared "main" scope ) 
				then raise (Multiple_declarations "main")
				else add_var {name="main"; v_type = [Unknown]} scope
			| Child(l,p) -> raise Main_wrong_scope


let first_pass list_decs = 
		let current_env = List.fold_left walk_decl global_env list_decs
			in print_string ((string_of_env current_env) ^ "\n"); current_env

(*let second_pass list_decs = *)


