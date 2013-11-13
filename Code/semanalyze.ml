open Sast
open Util

let type_mismatch var symtab = 
    let v = List.find (fun n -> n.name = var.name) symtab.identifiers
    in v.v_type <> var.v_type


(* Check if there are multiple type signatures for an id in the same scope *)
let rec mult_typesig id = function
    [] -> false
    | STypesig(x) :: rest -> if x.name = id then true else mult_typesig id rest
    | _ :: rest -> mult_typesig id rest

(* Checks if an id is in list of vars *)
let rec in_list x = function
    [] -> false
    | h::tl -> if x = h.name then true else in_list x tl

(* Only checks current scope (might not be needed) *)
let is_declared_here id symtab = List.exists (fun v -> v.name = id) symtab.identifiers

(* checks all scopes if id has been declared *)
let rec is_declared id symtab =
    try 
        List.exists (fun v -> v.name = id) symtab.identifiers
    with Not_found ->
        match symtab.parent with
            Some(parent) -> is_declared id parent
        |   _ -> false

(* Adds a var to a scope *)
let add_var v symtab = let s = v :: symtab.identifiers in
                       let symresult = {parent = symtab.parent; identifiers = s} in symresult

(* Start with an empty symbol table *)
let global_env = { identifiers = []; parent = None } 

(* Collect Variables in pattern *)
let rec collect_pat_vars = function
    [] -> []
    | (h::tl) -> match h with 
            Ast.Patvar(s) -> [s]
        | Ast.Patcomma(pl) -> collect_pat_vars pl
        | Ast.Patcons(pl1, pl2) -> 
            (collect_pat_vars [pl1]) @ (collect_pat_vars [pl2])
        | _ -> []
        @ collect_pat_vars tl

(*
let rec add_ids scope = function
    [] -> scope
    | (h::tl) -> let v = {name=h; v_type=[Ast.Unknown]} in 
                            add_ids (add_var v scope) tl 
*)
(* Returns a type from an expression*)
let rec get_type = function
      Ast.Literal(l) -> Ast.Unknown (* TInt or TBeat *)
    | Ast.Boolean(b) -> Ast.TBool
    | Ast.Variable(s) -> Ast.Unknown (* look up in symbol table? *)
    | Ast.Binop(e1, o, e2) -> Ast.Unknown (* Check type of operator *)
    | Ast.If(e1, e2, e3) -> (* Check both e2 and e3 and make sure the same *)
        let te1 = get_type e1 in 
        if te1 <> Ast.TBool then 
            type_error (" has type " ^
            " but is used as if it has type Bool")
        else let te2 = get_type e2 in 
             let te3 = get_type e3 in 
             if te2 <> te3 then
                type_error ( " has type "  ^
                " but " ^ " has type " )
                else te2
    | Ast.Beat(i1, i2) -> Ast.TBeat
    | Ast.Note(pc, reg, b) -> Ast.TNote
    | Ast.List(el) -> (* Check all elements have same type*)
        let hd = List.hd el in 
            let match_type_or_fail x y = 
                let tx = (get_type x) in 
                let ty = (get_type y) in 
                if tx <> ty 
                    then type_error ("elements in list have different types")
                else () in List.iter (match_type_or_fail hd) el; Ast.TList(get_type(hd))
    | Ast.Chord(el) -> (* Check all elements have type of TNote *)
        let hd = List.hd el in 
            let match_type_or_fail x y = 
                let tx = (get_type x) in 
                let ty = (get_type y) in 
                if tx <> ty 
                    then type_error ("elements in Chord should all have type of " ^
                    " but the element of "  ^
                    " has type of " )
                else () in List.iter (match_type_or_fail hd) el; Ast.TChord
    | Ast.System(el) -> (* Check all elements have type of TChord *)
        let hd = List.hd el in 
            let match_type_or_fail x y = 
                let tx = (get_type x) in 
                let ty = (get_type y) in 
                if tx <> ty 
                    then type_error ("elements in System should all have type of " ^ 
                     " but the element of "  ^
                    " has type of " )
                else () in List.iter (match_type_or_fail hd) el; Ast.TSystem
    | _ -> Ast.Unknown

(* First pass walk_decl -> Try to construct a symbol table *)
let walk_decl prog = function
    Ast.Tysig(id,types) -> (*print_string "type signature\n"; *)
                let func = {name=id; v_type = types} in 
                (*Check if we already have a type signature for this identifier in the
                current scope *)
                if (mult_typesig id prog.decls)
                    then raise (Multiple_type_sigs id)
                (* Check if we have bound this identifier to an expression whose type
                contradicts this signature *)
                else if (is_declared_here id prog.symtab && type_mismatch func prog.symtab)
                    then raise (Type_mismatch id)
                (* If type of signature matches that of bound expr, do nothing *)
                else if (is_declared_here id prog.symtab)
                    then {decls = prog.decls; symtab = prog.symtab}
                (* If identifier doesn't exist in current scope, add this type signature
                to the environment *)
                else {decls = prog.decls @ [STypesig(func)];
                symtab = (add_var func prog.symtab)}
                (*
    | Ast.Vardef(id, expr) -> (* print_string "var definition\n"; *)
                if( is_declared_here id prog.symtab) 
                    then raise (Multiple_declarations id)
                else 
                    { decls = prog.decls @ [SVardef(id, expr)];
                    symtab = (add_var 
                        {name=id; v_type = [get_type expr]} prog.symtab) } 
                    *)
                    (*
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
                *)
                    (*
    | Main(expr) -> print_string "main\n"; 
        match prog.symtab with 
            Parent(l, c) -> if( is_declared "main" prog.symtab ) 
                then raise (Multiple_declarations "main")
                else
                        { decls= prog.decls @ [SMain(expr)]; 
                        symtab = add_var {name="main"; v_type = [Unknown]} prog.symtab}
            | Child(l,p, c) -> raise Main_wrong_scope
            *)

