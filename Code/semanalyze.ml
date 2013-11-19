open Sast
open Ast
open Util

let rec types_to_s_type = function
      TInt -> Sast.Int
    | TBool -> Sast.Bool
    | TNote -> Sast.Note
    | TBeat -> Sast.Beat
    | TChord -> Sast.Chord
    | TSystem -> Sast.System
    | TList(l) -> Sast.List(types_to_s_type l)
    | TPoly(s) -> Sast.Poly(s)

let equiv_type v1 = match v1 with
      Sast.Num -> [Sast.Int; Sast.Beat; Sast.Num]
    | Sast.Chord -> [Sast.List(Sast.Note); Sast.Chord]
    | Sast.System -> [Sast.List(Sast.List(Sast.Note)); Sast.List(Sast.Chord); Sast.System]
    | x -> [x]

let rec diff_types v1 v2 = match v1, v2 with
    | x::t1, y::t2 -> if ((List.mem x (equiv_type y)) || (List.mem y (equiv_type x)))
                      then diff_types t1 t2 else true
    | [], [] -> false
    | [], _::_ -> true
    | _::_, [] -> true

let type_mismatch var symtab = 
    let v = List.find (fun n -> n.name = var.name) symtab.identifiers in
    let v1 = v.v_type in
    let v2 = var.v_type in
    let is_poly t = match t with
        | Sast.Poly(_) -> true
        | _ -> false
    in
    let poly_check = is_poly (List.hd v1) || is_poly (List.hd v2) in
    if (List.length v1 = 1 && List.length v2 = 1 && poly_check)
    then false
    else diff_types v1 v2


(* Check if a type signatures exists for an id in the current scope *)
let rec exists_typesig id = function
    [] -> false
    | STypesig(x) :: rest -> if x.name = id then true else exists_typesig id rest
    | _ :: rest -> exists_typesig id rest

(* Check if a definition exists for an id in the current scope *)
let rec exists_dec id = function
    [] -> false
    | SVardef(x, _) :: rest -> if x.name = id then true else exists_dec id rest
    | SFuncdec(f) :: rest -> if f.s_fname = id then true else exists_dec id rest
    | _ :: rest -> exists_dec id rest

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
let print_var = { name="print"; v_type = [Unknown] }
let random_var = { name = "random"; v_type = [Unknown] }
let global_env = { identifiers = [print_var; random_var]; parent = None } 

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

let rec add_ids scope = function
    [] -> scope
    | (h::tl) -> let v = {name=h; v_type=[Unknown]} in 
                            add_ids (add_var v scope) tl 

(* Returns a type from an expression*)
let rec get_type = function
      Ast.Literal(l) -> Num (* Int or Beat *)
    | Ast.Boolean(b) -> Bool
    | Ast.Variable(s) -> Unknown (* look up in symbol table? *)
    | Ast.Binop(e1, o, e2) ->  (* Check type of operator *)
        let te1 = get_type e1
        and te2 = get_type e2 in
            (match o with
                Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast. Mod |
                Ast.Less | Ast.Leq | Ast.Greater | Ast.Geq |
                Ast.BeatAdd | Ast.BeatSub | Ast.BeatDiv | Ast.BeatMul |
                Ast.BeatLess | Ast.BeatLeq | Ast.BeatGreater | Ast.BeatGeq |
                Ast.PCAdd | Ast.PCSub -> (* Arithmetic and Comparison Operators *)
                    if te1 <> Sast.Int 
                    then type_error ("First element of this binary operation " ^
                        "must be of type Int")
                    else
                        if te2 <> Sast.Int
                        then type_error ("Second element of this binary operation " ^
                            "must be of type Int")
                        else Sast.Int
                | Ast.And | Ast.Or ->  (* Boolean Operators: Bool && Bool, Bool || Bool *)
                    if te1 <> Sast.Bool
                    then type_error ("First element of this binary operation " ^
                        "must be of type Bool")
                    else
                        if te2 <> Sast.Bool
                        then type_error ("Second element of this binary operation " ^
                            "must be of type Bool")
                        else Sast.Bool
                | Ast.BoolEq -> (* Structural Comparision: Element == Element *)
                    if te1 <> te2
                    then type_error ("Elements must be of same type for " ^
                        "structural comparison")
                    else te1
                | Ast.Concat -> (* Concat: List ++ List *)
                    (* Not sure this checks the correct thing *)
                    if te1 <> Sast.List(get_type e1)
                    then type_error ("First element in a Concat expression " ^
                        "must be of type List")
                    else
                        (* Not sure this checks the correct thing *)
                        if te2 <> Sast.List(get_type e2)
                        then type_error ("Second element in a Concat expression " ^
                            "must be of type List")
                        else
                            if te2 <> te1
                            then type_error ("First and second element of a Concat " ^
                                "expression must be Lists of same type")
                            else te1
                | Ast.Cons -> (* Cons: Element : List *)
                    if te2 <> Sast.List(te1)
                    then type_error ("First element in a Cons expression " ^
                        "must be of same type as List in second element")
                    else te2
                | Ast.Trans -> (* Trans: Int ^^ List *)
                    if te1 <> Sast.Int
                    then type_error ("First element in a Trans expression " ^
                        "must be of type Int")
                    else
                        if te2 <> Sast.List(Sast.Int)
                        then type_error ("Second element in a Trans " ^
                            "expression must be a List of type Int")
                        else te2
            )
    | Ast.Prefix(o, e) -> (* Prefix Operators *)
        let te = get_type e in
        (match o with
            Ast.Not -> (* Not: ! Bool *)
                if te <> Sast.Bool
                    then type_error ("Element in Not operation but be of type Bool")
                else te
            | Ast.Inv | Ast.Retro -> (* Row Inversion: ~ List, Row Retrograde: <> List*)
                if te <> Sast.List(Sast.Int)
                    then type_error ("Element in Prefix operation " ^
                        "must be a List of type Int")
                else te
        )
    | Ast.If(e1, e2, e3) -> (* Check both e2 and e3 and make sure the same *)
        let te1 = get_type e1 in 
        if te1 <> Sast.Bool then 
            type_error (Ast.string_of_expr e1 ^ " has type " ^ string_of_s_type te1
            ^ " but is used as if it has type " ^ string_of_s_type Sast.Bool)
        else let te2 = get_type e2 in 
             let te3 = get_type e3 in 
             if te2 <> te3 then
                type_error (Ast.string_of_expr e2 ^ " has type " ^ string_of_s_type te2 
                ^ " but " ^ Ast.string_of_expr e3 ^ " has type " ^ string_of_s_type te3 
                ^ " which is not allowed in conditional statement")
                else te2
    | Ast.Beat(i1, i2) -> Sast.Beat
    | Ast.Note(pc, reg, b) -> Sast.Note
    | Ast.List(el) -> (* Check all elements have same type*)
        let hd = List.hd el in 
            let match_type_or_fail x y = 
                let tx = (get_type x) in 
                let ty = (get_type y) in 
                if tx <> ty 
                    then type_error (Ast.string_of_expr x ^ " has type of "
                        ^ Sast.string_of_s_type tx ^ " but "
                        ^ Ast.string_of_expr y ^ " has type " 
                        ^ Sast.string_of_s_type ty ^ " in a same list")
                else () in List.iter (match_type_or_fail hd) el; Sast.List(get_type(hd))
    | Ast.Chord(el) -> (* Check all elements have type of TNote *)
        let hd = List.hd el in 
            let match_type_or_fail x y = 
                let tx = (get_type x) in 
                let ty = (get_type y) in 
                if tx <> ty 
                    then type_error ("Elements in Chord should all have type of " 
                    ^ Ast.string_of_types Ast.TNote ^ " but the element of " 
                    ^ Ast.string_of_expr y ^ " has type of " ^ Sast.string_of_s_type ty)
                else () in List.iter (match_type_or_fail hd) el; 
        let hd = List.hd el in 
            let match_duration_or_fail x y = match x, y with
                 Ast.Note(p1,r1,bt1), Ast.Note(p2,r2,bt2) -> 
                    (if (Ast.string_of_expr bt1) <> (Ast.string_of_expr bt2) 
                        then type_error ("The time durating of " ^ Ast.string_of_expr bt1
                        ^ " is not the consistent with that of " ^ Ast.string_of_expr bt2)
                        else ())
               | _,_ -> type_error ("Not Expected Exception")
        in List.iter (match_duration_or_fail hd) el; Sast.Chord
    | Ast.System(el) -> (* Check all elements have type of TChord *)
        let hd = List.hd el in 
            let match_type_or_fail x y = 
                let tx = (get_type x) in 
                let ty = (get_type y) in 
                if tx <> ty 
                    then type_error ("Elements in Chord should all have type of " 
                    ^ string_of_s_type Sast.Chord ^ " but the element of " 
                    ^ Ast.string_of_expr y ^ " has type of " ^ string_of_s_type ty)
                else () in List.iter (match_type_or_fail hd) el; Sast.System
    | _ -> Sast.Unknown

(* First pass walk_decl -> Try to construct a symbol table *)
let walk_decl prog = function
    Ast.Tysig(id,types) -> (*print_string "type signature\n"; *)
                let func = {name=id; v_type = (List.map types_to_s_type types)} in 
                if (exists_typesig id prog.decls)
                    then raise (Multiple_type_sigs id)
                (* Check if we have bound this identifier to an expression whose type
                contradicts this signature *)
                else if (is_declared_here id prog.symtab && type_mismatch func prog.symtab)
                    then raise (Type_mismatch id)
                (* Add type signature to current environment *)
                else {decls = prog.decls @ [STypesig(func)];
                      symtab = (add_var func prog.symtab)}
    | Ast.Vardef(id, expr) -> 
                let var = {name=id; v_type = [get_type expr]} in
                if(exists_dec id prog.decls) 
                    then raise (Multiple_declarations id)
                (* Only worry if tysig given in same scope doesn't match this var's type *)
                else if(is_declared_here id prog.symtab && type_mismatch var prog.symtab)
                    then raise (Type_mismatch id)
                else 
                    { decls = prog.decls @ [SVardef(var, expr)];
                    symtab = (add_var var prog.symtab) } 

    | Main(expr) -> print_string "main\n"; 
        match prog.symtab.parent with 
            Some(_) -> raise Main_wrong_scope
            | _ -> if( is_declared "main" prog.symtab ) 
                   then raise (Multiple_declarations "main")
                   else { decls= prog.decls @ [SMain(expr)]; 
                          symtab = add_var {name="main"; v_type = [Unknown]} prog.symtab}
    | _ -> prog
    (*
    | Ast.Funcdec(fdec) ->
            
            let new_scope = {parent = Some(prog.symtab); identifiers = []} in
            let f_vars = collect_pat_vars fdec.args in 
            let new_scope = add_ids new_scope f_vars in
            let types = if is_declared_here fdec.fname prog.symtab 
            (* Currently checking this scope need to check higher too *)
                        then get_func_type fdec.fname prog.symtab
                        else [Ast.Unknown] (* Need to get types of args and expr *)
                        in 
            let funcdef = SFuncdec({s_fname = fdec.fname; 
                                            type_sig = types;
                                            s_args = fdec.args;
                                            s_value = fdec.value;
                                            scope = new_scope;}) in 
            let var = {name = funcdef.s_fname; v_type = funcdef.type_sig} in
                { decls = prog.decls @ [funcdef]; symtab = global }
                *)

(* Right now gets called by smurf *)

let first_pass list_decs = 
    let program = List.fold_left walk_decl {decls=[]; symtab = global_env} list_decs
    in (print_string (string_of_s_program program)); program
