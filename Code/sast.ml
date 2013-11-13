open Ast
open Util

exception Multiple_declarations of string
exception Multiple_type_sigs of string
exception Type_mismatch of string
exception Main_wrong_scope

exception Type_error of string
let type_error msg = raise (Type_error msg)

type symbol_table = {
    parent : symbol_table option;
    variables : s_var_decl list;
    functions : s_func_decl list;
}

type s_func_decl = {
    s_fname : string; 
    type_sig : types list;
    s_args :  pattern list;
    s_value : expr;
    scope : symbol_table;
}

type s_dec = 
      STypesig of string * types list
    | SFuncdec of s_func_decl
    | SVardef of string * expr
    | SMain of expr 

type s_program = {
    decls : s_dec list;
    symtab : symbol_table;
}

let string_of_var v = 
    "Var: " ^ v.name ^ " :: " ^ String.concat " -> " (List.map Ast.string_of_types v.v_type) ^ "\n"


let rec string_of_env symtab = match symtab.parent
    if symtab.parent "Global Scope: \n\t" ^ 
        String.concat "\t" (List.map string_of_var v) ^ "\n\t" ^
        String.concat "\n\t" (List.map string_of_env c) ^ "\n"
    | Child(v,p, c) -> (*(string_of_env p) ^ *)"\tNew Scope: " ^ 
        String.concat "\n\t" (List.map string_of_var v) ^"\n\t"

let type_mismatch var = function
    Parent(vlist, _) -> let v = List.find (fun n -> n.name = var.name) vlist
                        in v.v_type <> var.v_type
    | Child(vlist,_,_) -> let v = List.find (fun n-> n.name = var.name) vlist
                        in v.v_type <> var.v_type


(* Check if there are multiple type signatures for an id in the same scope *)
let rec mult_typesig id = function
    [] -> false
    | STypesig(x, _) :: rest -> if x = id then true else mult_typesig id rest
    | _ :: rest -> mult_typesig id rest

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
    | If(e1, e2, e3) -> (* Check both e2 and e3 and make sure the same *)
        let te1 = get_type e1 in 
        if te1 <> TBool then 
            type_error (string_of_expr e1 ^ " has type " ^ string_of_types te1 ^ " but is used as if it has type Bool")
        else let te2 = get_type e2 in 
             let te3 = get_type e3 in 
             if te2 <> te3 then
                type_error (string_of_expr e2 ^ " has type " ^ string_of_types te2 ^ " but " ^ string_of_expr e3 ^ " has type " ^ string_of_types te3)
                else te2
    | Beat(i1, i2) -> TBeat
    | Note(pc, reg, b) -> TNote
    | List(el) -> (* Check all elements have same type*)
        let hd = List.hd el in 
            let match_type_or_fail x y = 
                let tx = (get_type x) in 
                let ty = (get_type y) in 
                if tx <> ty 
                    then type_error ("elements in list have different types")
                else () in List.iter (match_type_or_fail hd) el; TList(get_type(hd))
    | Chord(el) -> (* Check all elements have type of TNote *)
        let hd = List.hd el in 
            let match_type_or_fail x y = 
                let tx = (get_type x) in 
                let ty = (get_type y) in 
                if tx <> ty 
                    then type_error ("elements in Chord should all have type of " ^ string_of_types TNote ^ " but the element of " ^ string_of_expr y ^ " has type of " ^ string_of_types ty)
                else () in List.iter (match_type_or_fail hd) el; TChord
    | System(el) -> (* Check all elements have type of TChord *)
        let hd = List.hd el in 
            let match_type_or_fail x y = 
                let tx = (get_type x) in 
                let ty = (get_type y) in 
                if tx <> ty 
                    then type_error ("elements in System should all have type of " ^ string_of_types TChord ^ " but the element of " ^ string_of_expr y ^ " has type of " ^ string_of_types ty)
                else () in List.iter (match_type_or_fail hd) el; TSystem
    | _ -> Unknown

(* First pass walk_decl -> Try to construct a symbol table *)
let walk_decl prog = function
    Tysig(id,types) -> (*print_string "type signature\n"; *)
                let func = {name=id; v_type = types} in 
                (*Check if we already have a type signature for this identifier in the current scope *)
                if (mult_typesig id prog.decls)
                    then raise (Multiple_type_sigs id)
                (* Check if we have bound this identifier to an expression whose type contradicts this signature *)
                else if (is_declared_here id prog.symtab && type_mismatch func prog.symtab)
                    then raise (Type_mismatch id)
                (* If type of signature matches that of bound expr, do nothing *)
                else if (is_declared_here id prog.symtab)
                    then {decls = prog.decls; symtab = prog.symtab}
                (* If identifier doesn't exist in current scope, add this type signature to the environment *)
                else {decls = prog.decls @ [STypesig(id, types)]; symtab = (add_var func prog.symtab)}
    | Vardef(id, expr) -> (* print_string "var definition\n"; *)
                if( is_declared_here id prog.symtab) 
                    then raise (Multiple_declarations id)
                else 
                    { decls = prog.decls @ [SVardef(id, expr)];
                    symtab = (add_var 
                        {name=id; v_type = [get_type expr]} prog.symtab) } 
    | Funcdec(fdec) ->  (*print_string "function declaration\n";*)
        let new_scope = Child([], prog.symtab, []) in
            let f_vars = collect_pat_vars fdec.args in 
            let new_scope = add_ids new_scope f_vars in
            let global = (add_child new_scope prog.symtab) in 
            let funcdef = SFuncdec({s_fname = fdec.fname; 
                                            type_sig = [Unknown];
                                            s_args = fdec.args;
                                            s_value = fdec.value;
                                            scope = new_scope;}) in 
                { decls = prog.decls @ [funcdef]; symtab = global }
    | Main(expr) -> print_string "main\n"; 
        match prog.symtab with 
            Parent(l, c) -> if( is_declared "main" prog.symtab ) 
                then raise (Multiple_declarations "main")
                else
                        { decls= prog.decls @ [SMain(expr)]; 
                        symtab = add_var {name="main"; v_type = [Unknown]} prog.symtab}
            | Child(l,p, c) -> raise Main_wrong_scope

(* Right now gets called by smurf *)
let first_pass list_decs = 
        let program = List.fold_left walk_decl {decls=[]; symtab = global_env} list_decs
            in (print_string (string_of_env program.symtab)); program

let walk_decl_2 scope = function
    _ -> scope


let second_pass list_decs current_env =
    let current_env = List.fold_left walk_decl_2 current_env list_decs
     in print_string ((string_of_env current_env)^ "\n"); current_env
