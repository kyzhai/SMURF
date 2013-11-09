open Ast

exception Multiple_declarations of string
exception Main_wrong_scope

(* entry in symbol table *)
type var = {
		name : string;
		v_type : Ast.types list;
	}

(* Symbol Table *)
(* Used a list but might want to use a Map module for lookup time *)
(* Doubly linked list needed right now *)
(* Parent(variables, children) *)
(* Child(variables, parent, children *)
type env =
	Parent of var list * env list
	| Child of var list * env * env list

type s_func_decl = {
	fname : string; 
	type_sig : types list;
	args :  pattern list;
	value : expr;
	symtab : env;
}

type s_dec = 
	(*	STypeSig of string * types list // don't need anymore *)
	  SFuncdec of func_decl
	| SVardef of string * expr
	| SMain of expr 

type program = {
	decls : s_dec list;
	symtab : env;
}

let string_of_var v = 
	"Var: " ^ v.name ^ " :: " ^ String.concat " -> " (List.map Ast.string_of_types v.v_type) ^ "\n"


let rec string_of_env = function
	Parent(v, c) -> "Global Scope: \n\t" ^ 
		String.concat "\t" (List.map string_of_var v) ^ "\n\t" ^
		String.concat "\n\t" (List.map string_of_env c) ^ "\n"
	| Child(v,p, c) -> (*(string_of_env p) ^ *)"\tNew Scope: " ^ 
		String.concat "\n\t" (List.map string_of_var v) ^"\n\t"

(* Checks if an id is in list of vars *)
let rec in_list x = function
	[] -> false
	| h::tl -> if x = h.name then true else in_list x tl

(* Only checks current scope (might not be needed) *)
let is_declared_here id = function
	Parent(l, c) -> in_list id l
	| Child(l, p, c) -> in_list id l

(* checks all scopes if id has been declared *)
let rec is_declared id = function
	Parent(l, c) -> in_list id l
	| Child(l, p, c) -> if (in_list id l) then true else is_declared id p 

(* Adds a var to a scope *)
let add_var v = function
	Parent(l, c) -> Parent(v :: l, c)
	| Child(l, p, c) -> Child(v :: l, p, c)

let add_child child = function
	Parent(l, c) -> Parent(l, child::c)
	| Child(l, p, c) -> Child(l, p, child::c)

(* Start with an empty symbol table *)
let global_env = Parent([], [])

(* Collect Variables in pattern *)
let rec collect_pat_vars = function
	[] -> []
	| (h::tl) -> match h with 
			Patvar(s) -> [s]
		| Patcomma(pl) -> collect_pat_vars pl
		| Patcons(pl1, pl2) -> 
			(collect_pat_vars [pl1]) @ (collect_pat_vars [pl2])
		| _ -> []
		@ collect_pat_vars tl

let rec add_ids scope = function
	[] -> scope
	| (h::tl) -> let v = {name=h; v_type=[Unknown]} in 
							add_ids (add_var v scope) tl 
(* Returns a type from an expression*)
let rec get_type = function
		Literal(l) -> Unknown (* TInt or TBeat *)
	| Boolean(b) -> TBool
	| Variable(s) -> Unknown (* look up in symbol table? *)
	| Binop(e1, o, e2) -> Unknown (* Check type of operator *)
	| If(e1, e2, e3) -> Unknown (* Check both e2 and e3 and make sure the same *)
	| Beat(i1, i2) -> TBeat
	| Note(pc, reg, b) -> TNote
	| List(el) -> TList(get_type (List.hd el))
	| Chord(el) -> TChord
	| System(el) -> TSystem
	| _ -> Unknown

(* First pass walk_decl -> Try to construct a symbol table *)
let walk_decl prog = function
		Tysig(id,types) -> (*print_string "type signature\n"; *)
				let func = {name=id; v_type = types} in 
				if (is_declared_here id prog.symtab) 
					then raise (Multiple_declarations id)
				else {decls = prog.decls; symtab = (add_var func prog.symtab)}
	| Vardef(id, expr) -> (*print_string "var definition\n"; *)
				(*if( is_declared_here id scope) 
					then raise (Multiple_declarations id)
				else add_var {name=id; v_type = [get_type expr]} scope *)
				prog
	| Funcdec(fdec) ->  (*print_string "function declaration\n";*)
		(*let new_scope = Child([], scope, []) in
			let f_vars = collect_pat_vars fdec.args in 
			let new_scope = add_ids new_scope f_vars in
			(add_child new_scope scope) *)
			prog
	| Main(expr) -> (* print_string "main\n"; *)
		(* match scope with 
			Parent(l, c) -> if( is_declared "main" scope ) 
				then raise (Multiple_declarations "main")
				else add_var {name="main"; v_type = [Unknown]} scope
			| Child(l,p, c) -> raise Main_wrong_scope *)
			prog

(* Right now gets called by smurf *)
let first_pass list_decs = 
		let program = List.fold_left walk_decl {decls=[]; symtab = global_env} list_decs
			in (print_string (string_of_env program.symtab));program

let walk_decl_2 scope = function
	_ -> scope


let second_pass list_decs current_env =
	let current_env = List.fold_left walk_decl_2 current_env list_decs
	 in print_string ((string_of_env current_env)^ "\n"); current_env

