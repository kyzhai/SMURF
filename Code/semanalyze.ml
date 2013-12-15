open Sast
open Ast
open Util

module StringMap =Map.Make(String)

let rec types_to_s_type = function
      TInt -> Sast.Int
    | TBool -> Sast.Bool
    | TNote -> Sast.Note
    | TBeat -> Sast.Beat
    | TChord -> Sast.Chord
    | TSystem -> Sast.System
    | TList(l) -> Sast.List(types_to_s_type l)
    | TPoly(s) -> Sast.Poly(s)

(* Return a list of equivalent types to v1 *)
let equiv_type v1 = match v1 with
    Sast.Chord -> [Sast.List(Sast.Note); Sast.Chord]
    | Sast.System -> [Sast.List(Sast.List(Sast.Note)); Sast.List(Sast.Chord); Sast.System]
    | x -> [x]


(* Return true if v1 and v2 are different types *)
let rec diff_types v1 v2 = match v1, v2 with
    | Sast.List(x)::t1, Sast.List(y)::t2 -> diff_types (x::t1) (y::t2)
    | x::t1, y::t2 -> if ((List.mem x (equiv_type y)) || (List.mem y (equiv_type x)))
                      then diff_types t1 t2 else true
    | [], [] -> false
    | [], _::_ -> true
    | _::_, [] -> true


(* Check if an int is a valid beat *)
let beat_as_int value = if List.mem value [1;2;4;8;16] then true else false

(* Returns true if two types are just ints, beats, or nested ints or beats wher the number of nestings for
  both types is equivalent *)
let rec beats_and_ints ty1 ty2 = match ty2, ty2 with
    Sast.List(t1), Sast.List(t2) -> beats_and_ints t1 t2
   | Sast.Beat, Sast.Int -> true
   | Sast.Int, Sast.Beat -> true
   | Sast.Int, Sast.Int -> true
   | Sast.Beat, Sast.Beat -> true
   | _, _ -> false

(* Check if we have a Beat expression in a list of s_exprs *)
let rec contains_beat = function
    [] -> false
    | SList(sexpr)::rest -> if contains_beat sexpr then true else contains_beat rest
    | SBeat(_,_)::rest -> true
    | _::rest -> contains_beat rest

(* If an Int is in the given list of s_exprs, make sure it's a power of two and return Beat type if so *)
let rec powers_of_two = function
    | [] -> Sast.Beat
    | SList(sexpr) :: rest -> Sast.List(powers_of_two (sexpr @ 
                                            (let rec delist = function
                                             [] -> []
                                             |SList(sexpr)::r -> sexpr @ delist r
                                             |_ -> type_error ("Found a list of nested elements
                                                                with non-equal number of nestings")
                                             in delist rest)))
    | SLiteral(i) :: rest -> if beat_as_int i then powers_of_two rest else
                                type_error ("Non-power of 2 entity " ^ (string_of_int i) ^
                                            " in list of beat elements")
    | _ :: rest -> powers_of_two rest



(* Return true if argument is a system type or a nested system *)
let rec eventual_system = function 
    Sast.System | Sast.List(Sast.Chord) | Sast.List(Sast.List(Sast.Note)) -> true
   | Sast.List(x) -> if List.mem x (equiv_type Sast.System) then true else
                        eventual_system x
   | _ -> false

(* Check if a type signatures exists for an id in the current scope *)
let rec exists_typesig id = function
    [] -> false
    | sym_entry :: rest -> if sym_entry.name = id then
                            if sym_entry.v_type <> [Unknown] then true
                            else false
                           else exists_typesig id rest

(* Get the type signature for an identifier in the current scope *)
let get_typesig id ids = (List.find (fun t -> t.name = id) ids).v_type

(* Get type signature for function id in current or higher scope *)
let rec get_types_p id symtab = 
    if exists_typesig id symtab.identifiers then get_typesig id symtab.identifiers
    else match symtab.parent with
        | Some(psym) -> get_types_p id psym
        | None -> raise (No_type_signature_found id)

(* Check if a vardef or funcdec exists for an id in the current scope *)
let rec exists_dec id ty = function
    [] -> false
    | SVardef(x, _) :: rest -> if x.name = id then true else exists_dec id ty rest
    | SFuncdec(f) :: rest -> (match ty with 
                             "func" -> exists_dec id ty rest
                             | _ -> if f.s_fname = id then true else exists_dec id ty rest)
    | _ :: rest -> exists_dec id ty rest

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

(* Add new entry into symbol table or modify existing one if necessary (First Pass work) *)
let mod_var entry symtab =
    if is_declared_here entry.name symtab then 
        let preventries = List.filter (fun v -> v.name = entry.name) symtab.identifiers in
        let newsym = List.filter (fun v -> v.name <> entry.name) symtab.identifiers in
        let firsten = List.hd preventries in
        match entry with
        (* Entry is type signature *)
        {v_expr = None } -> 
            if List.length preventries = 1 then
                let newen = {name = entry.name; pats = firsten.pats; v_type = entry.v_type;
                             v_expr = firsten.v_expr} in newen :: newsym
            else let newens = List.map (fun en -> let result = {name = en.name; pats = en.pats;
                                                                v_type = entry.v_type; 
                                                                v_expr = en.v_expr} in result) 
                                        preventries in newens @ newsym
        (* Entry is vardef *)
        | {pats = [] } ->
            let newen = {name = entry.name; pats = entry.pats; v_type = firsten.v_type;
                         v_expr = entry.v_expr} in newen :: newsym
        (* Entry is funcdec *)
        | _ -> 
            let newen = {name = entry.name; pats = entry.pats; v_type = firsten.v_type;
                             v_expr = entry.v_expr} in 
            if List.length preventries = 1 && firsten.v_expr = None then newen :: newsym
            else newen :: symtab.identifiers
    else entry :: symtab.identifiers

(* Update type of variable definition in our symbol table and our list of declarations *)
let replace_vardef program var oldvar = match var with
    | SVardef(ids, s_expr) -> 
        let newdecls = List.filter (fun dec -> dec != oldvar) program.decls in
        let newsym = List.filter (fun v -> v.name <> ids.name) program.symtab.identifiers in
        let newentry = {name = ids.name; pats = []; v_type = ids.v_type; v_expr = ids.v_expr} in
        program.symtab.identifiers <- newentry :: newsym;
        program.decls <- (var :: newdecls); program
    | _ -> program
        


(* program->string->s_func_decl *)
let rec find_f_def program f_name = 
	let decl = List.filter 
		(fun dec -> 
			match dec with SFuncdec(x)-> x.s_fname = f_name | _ -> false) 
			program.decls  in decl
			(*with Not_found ->  raise (Function_not_defined f_name) in 
				match  decl with 
					SFuncdec(x) -> x
					| _ -> raise (Function_not_defined f_name)
			*)

(* Update type and scope of function declaration in our symbol table and our list of declarations *)
let replace_funcdec program func oldfunc = match func with
    | SFuncdec(info) -> 
        let newdecls = List.filter (fun dec -> dec <> oldfunc) program.decls in
        let newsym = List.filter (fun v -> v.name <> info.s_fname || v.pats <> info.s_args) 
                     program.symtab.identifiers in
        let newentry = {name = info.s_fname; v_type = info.type_sig; 
                        pats = info.s_args; v_expr = Some(info.s_value)} in
        program.symtab.identifiers <- newentry :: newsym;
        program.decls <- (func :: newdecls); program
    | _ -> program

let replace_main program new_main = 
	let newsym = List.filter (fun v -> v.name <> new_main.name) program.symtab.identifiers in
	program.symtab.identifiers <- new_main :: newsym;
	program

(* Start with an empty symbol table *)
let print_var = { name="print"; 
                  pats = [Patvar("x")]; 
                  v_type = [Poly("a"); Poly("a")]; 
                  v_expr = Some(SPrint(SVariable("x")))}
let random_var = { name = "random"; 
                   pats = [];  
                   v_type = [Int]; 
                   v_expr = Some(SRandom) }
let head_var = { name = "head"; 
                 pats = [Patcons(Patvar("h"), Patvar("t"))]; 
                 v_type = [Sast.List(Poly("a"));Poly("a")]; 
                 v_expr = Some(SVariable("h"))}
let tail_var = { name = "tail"; 
                 pats = [Patcons(Patvar("h"), Patvar("t"))]; 
                 v_type = [Sast.List(Poly("a"));Sast.List(Poly("a"))]; 
                 v_expr = Some(SVariable("t"))}
let global_env = { identifiers = [print_var; random_var; head_var; tail_var]; parent = None } 

(* Check if a type is just a bunch of nested empty lists *)
let rec only_empties = function
    Sast.Empty -> true
    | Sast.List(x) -> only_empties x
    | _ -> false

(* So far, just used to check for pattern errors in collect_pat_vars *)
let rec get_pat_type = function
    Patconst(_) -> Sast.Int
    | Patbool(_)-> Sast.Bool
    | Patvar(_)| Patwild -> Sast.Unknown
    | Patcomma l -> if l = [] then Sast.List(Empty)
                    else let hd = List.hd l in 
                     let match_type_or_fail x y = 
                        let tx = (get_pat_type x) in
                        let ty = (get_pat_type y) in
                        if tx <> ty && tx <> Sast.Unknown && ty <> Sast.Unknown then 
                            raise (Pattern_list_type_mismatch 
                                  (string_of_s_type tx ^ " doesn't match " ^ string_of_s_type ty))
                        else () in List.iter (match_type_or_fail hd) l; Sast.List(get_pat_type hd)
    | Patcons (e1, e2) -> 
        let ty1 = get_pat_type e1 in
        let ty2 = get_pat_type e2 in
        (match ty2 with
            Sast.Unknown -> Sast.List(ty1)
            | Sast.List(els) -> if only_empties els then Sast.List(ty1)
                                else if ty1 <> els && ty1 <> Sast.Unknown && els <> Sast.Unknown
                                     then raise (Pattern_list_type_mismatch (string_of_s_type ty1
                                                ^ " doesn't match " ^ string_of_s_type els))
                                else if ty1 <> Sast.Unknown then Sast.List(ty1)
                                else Sast.List(els)
    | _ -> raise (Cons_pattern_type_mismatch (string_of_patterns e2)))



(* Collect Variables in pattern *)
let rec collect_pat_vars = function
    [] -> []
    | Patvar(s) :: rest -> s :: collect_pat_vars rest
    | (Patcomma(pl) as l) :: rest -> (match (get_pat_type l) with _ ->  collect_pat_vars pl)
                                     @ collect_pat_vars rest
    | (Patcons(pl1, pl2) as c) :: rest -> (match (get_pat_type c) with _ -> 
                                          ((collect_pat_vars [pl1]) @ (collect_pat_vars [pl2])))
                                          @ collect_pat_vars rest
    | _ :: rest -> collect_pat_vars rest

(* Check if there exist 2 function declarations with the same ids and pattern lists *)
let rec same_pats func = function
    [] -> false
    | SFuncdec(info) :: rest -> 
        if (info.s_fname <> func.s_fname) then same_pats func rest
        else if (List.length info.s_args <> List.length func.s_args) then same_pats func rest
        else          let rec compare_pats arg1 arg2 = match arg1, arg2 with
            | Patconst(x), Patconst(y) -> if x <> y then false else true
            | Patbool(x), Patbool(y) -> if x <> y then false else true
            | Patvar(_), Patvar(_) -> true
            | Patwild, Patwild -> true
            | Patcomma(l1), Patcomma(l2) -> 
                if (List.length l1 <> List.length l2) then false else
                if (List.length l1 = 0 && List.length l2 = 0) then true else
                if (List.length l1 = 0 || List.length l2 = 0) then false else
                if (List.for_all (fun v -> v = true) (List.map2 compare_pats l1 l2))
                then true else false
            | Patcons(p1, p2), Patcons(p3, p4) ->
                if (compare_pats p1 p3 && compare_pats p2 p4) then true else false
            | Patcomma(l1), Patcons(p1, p2) | Patcons(p1, p2), Patcomma(l1) ->
                if (List.length l1 = 0) then false else
                if (compare_pats (List.hd l1) p1) then compare_pats (Patcomma(List.tl l1)) p2
                else false
            | _, _ -> false
            in let result = List.map2 compare_pats info.s_args func.s_args in
                List.for_all (fun v -> v = true) result
    | _ :: rest -> same_pats func rest


(* Set up a new scope given a set of variables to put into scope *)
let rec gen_new_scope = function
    [] -> []
    | pat :: rest -> if List.exists (fun p -> p = pat) rest then raise (Multiple_patterns pat)
                     else {name = pat; pats = []; v_type = [Unknown];
                           v_expr = None} :: gen_new_scope rest

let rec find_var_entry symtab v =
	try ( List.find (fun t -> t.name = v) symtab.identifiers)
		with Not_found -> (match symtab.parent with 
			Some(p) -> find_var_entry p v 
			| None -> raise (Missing_variable_definition v))
			
let change_type symtab old_var n_type = 
	let new_var = {name = old_var.name; 
								pats = old_var.pats;
								v_type = [n_type];
								v_expr = old_var.v_expr} in
	let other_vars = List.filter 
		(fun vs -> vs.name <> old_var.name)
		symtab.identifiers in 
	{ parent = symtab.parent; identifiers = new_var :: other_vars}
	


(* Returns a type from an expression*)
let rec get_type  program = function
      SLiteral(l) -> Int
    | SBoolean(b) -> Bool
    | SVariable(s) -> 
			let var = find_var_entry program.symtab s in 
				let ts = var.v_type in 
				if(List.length ts <> 1) then raise (Function_used_as_variable s)
				else let t = List.hd ts in 
					if(t <> Unknown) then t
					else  
						(match var.v_expr with 
						Some(expr) -> 
							let n_prog = {decls = program.decls; 
														symtab = (change_type program.symtab var Still_unknown)} in 
							get_type  n_prog expr
						| None -> raise (Missing_variable_definition s))
    | SBinop(e1, o, e2) ->  (* Check type of operator *)
        let te1 = get_type program e1
        and te2 = get_type program e2 in
            (match o with
                Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast. Mod |
                Ast.PCAdd | Ast.PCSub ->
                (* Arithmetic Operators *)
                    if te1 <> Sast.Int
                    then type_error ("First element of an arithmetic binary operation " ^
                        "must be of type Int but element was of type " ^
                        Sast.string_of_s_type te1)
                    else
                        if te2 <> Sast.Int
                        then type_error ("Second element of an arithmetic binary operation " ^
                            "must be of type Int but element was of type " ^
                            Sast.string_of_s_type te2)
                        else Sast.Int
                | Ast.Less | Ast.Leq | Ast.Greater | Ast.Geq ->
                  (* Comparison Operators *)
                    if te1 <> Sast.Int
                    then type_error ("First element of a comparison binary operation " ^
                        "must be of type Int but element was of type " ^
                        Sast.string_of_s_type te1)
                    else
                        if te2 <> Sast.Int
                        then type_error ("Second element of a comparison binary operation " ^
                            "must be of type Int but element was of type " ^
                            Sast.string_of_s_type te2)
                        else Sast.Int
                | Ast.BeatAdd | Ast.BeatSub | Ast.BeatDiv | Ast.BeatMul ->
                  (* Beat Arithmetic Operators *)
                    if te1 <> Sast.Int && te1 <> Sast.Beat
                    then type_error ("First element of a Beat arithmetic binary " ^
                        "operation must be of types Int or Beat but element was of type " ^
                        Sast.string_of_s_type te1)
                    else
                        if te2 <> Sast.Int && te2 <> Sast.Beat
                        then type_error ("Second element of a Beat arithmetic binary " ^
                            "operation must be of types Int or Beat but element was of type " ^
                            Sast.string_of_s_type te2)
                        else Sast.Beat
                | Ast.BeatLess | Ast.BeatLeq | Ast.BeatGreater | Ast.BeatGeq ->
                  (* Beat Comparison Operators *)
                    if te1 <> Sast.Int && te1 <> Sast.Beat
                    then type_error ("First element of a Beat comparison binary " ^
                        "operation must be of types Int or Beat but element was of type " ^
                        Sast.string_of_s_type te1)
                    else
                        if te2 <> Sast.Int && te2 <> Sast.Beat
                        then type_error ("Second element of a Beat comaprison binary " ^
                            "operation must be of types Int or Beat but element was of type " ^
                            Sast.string_of_s_type te2)
                        else Sast.Beat
                | Ast.And | Ast.Or ->  (* Boolean Operators: Bool && Bool, Bool || Bool *)
                    if te1 <> Sast.Bool
                    then type_error ("First element of a boolean binary operation " ^
                        "must be of type Bool but element was of type " ^
                        Sast.string_of_s_type te1)
                    else
                        if te2 <> Sast.Bool
                        then type_error ("Second element of a boolean binary operation " ^
                            "must be of type Bool but element was of type " ^
                            Sast.string_of_s_type te2)
                        else Sast.Bool
                | Ast.BoolEq -> (* Structural Comparision: Element == Element *)
                    if te1 <> te2
                    then type_error ("Elements must be of same type for " ^
                        "structural comparison. First element has type " ^
                        Sast.string_of_s_type te1 ^ " and second element has type " ^
                        Sast.string_of_s_type te2)
                    else te1
                | Ast.Concat -> (* Concat: List ++ List *)
                    (* Not sure this checks the correct thing *)
                    (match te1 with 
                      Sast.List(t1) -> (match te2 with
                          Sast.List(t2) -> (if t1 <> t2 then 
                              type_error ("Operands of a concat operator have different types") else te1)
                        | Sast.Empty -> te1
                        | _ -> type_error "Concat operator can only used between lists")
                    | Sast.Chord -> (match te2 with
                          Sast.Chord | Sast.Empty | Sast.List(Sast.Note) -> Sast.Chord
                        | _ -> type_error ("Operands of a concat operator have different types"))
                    | Sast.System -> (match te2 with
                          Sast.System | Sast.List(Sast.Chord) | Sast.List(Sast.List(Sast.Note)) 
                          | Sast.Empty -> Sast.System
                        | _ -> type_error ("Operands of a concat operator have different types"))
                    | Sast.Empty -> (match te2 with
                          Sast.List(t2) -> te2
                        | Sast.Empty -> Sast.Empty
                        | Sast.Chord -> Sast.Chord
                        | Sast.System -> Sast.System
                        | _ -> type_error "Concat operator can only used between lists")
                    | _ -> type_error "Concat operator can only used between lists")

                | Ast.Cons -> (* Cons: Element : List *)
                    (match te2 with 
                       Sast.List(t2) -> (if te1 <> t2 then 
                            type_error ("The types of the lhs and rhs of a cons operator don't match")
                            else te2)
                     | Sast.Chord -> (if te1 <> Sast.Note then 
                         type_error ("The types of the lhs and rhs of a cons operator don't match")
                         else te2)
                     | Sast.System -> (if te1 <> Sast.Chord && te1 <> Sast.List(Sast.Note)  then 
                         type_error ("The types of the lhs and rhs of a cons operator don't match")
                         else te2)
                     | Sast.Empty -> (match te1 with
                           Sast.Note -> Sast.Chord
                         | Sast.Chord -> Sast.System 
                         | _ -> Sast.List(te1))
                     | _ -> type_error ("The second operand of a cons operator was: " ^ (Sast.string_of_s_type te2) ^ ", but a type of list was expected"))
                | Ast.Trans -> (* Trans: Int ^^ List *)
                    if te1 <> Sast.Int
                    then type_error ("First element in a Trans expression " ^
                        "must be of type Int but element was of type " ^
                        Sast.string_of_s_type te1)
                    else
                        if te2 <> Sast.List(Sast.Int)
                        then type_error ("Second element in a Trans expression " ^
                            "must be a List of type Int but element was of type " ^
                            Sast.string_of_s_type te2)
                        else te2
            )
    | SPrefix(o, e) -> (* Prefix Operators *)
        let te = get_type program e in
        (match o with
            Ast.Not -> (* Not: ! Bool *)
                if te <> Sast.Bool
                then type_error ("Element in Not operation must be of type Bool " ^
                    "but element was of type " ^ Sast.string_of_s_type te)
                else te
            | Ast.Inv | Ast.Retro -> (* Row Inversion: ~ List, Row Retrograde: <> List*)
                if te <> Sast.List(Sast.Int)
                then type_error ("Element in Prefix operation must be a List of " ^
                    "type Int but element was of type " ^ Sast.string_of_s_type te)
                else te
        )
    | SIf(e1, e2, e3) -> (* Check both e2 and e3 and make sure the same *)
        let te1 = get_type program e1 in 
        if te1 <> Sast.Bool then 
            type_error (string_of_sexpr e1 ^ " has type " ^ string_of_s_type te1
            ^ " but is used as if it has type " ^ string_of_s_type Sast.Bool)
        else let te2 = get_type program e2 in 
             let te3 = get_type program e3 in 
             if te2 <> te3 then
                type_error (string_of_sexpr e2 ^ " has type " ^ string_of_s_type te2 
                ^ " but " ^ string_of_sexpr e3 ^ " has type " ^ string_of_s_type te3 
                ^ " which is not allowed in conditional statement")
                else te2
    | SBeat(i1, i2) -> 
        let ti1 = get_type program i1 in
        if ti1 <> Sast.Int
        then type_error ("First element in a Beat must be of type Int " ^
            "and a power of 2 between 1 and 16. The given element was of type " ^
            Sast.string_of_s_type ti1)
        else
          (* Need to check more thoroughly*)
          if i2 < 0 || i2 > 4
            then type_error ("Dots may not increase Beat value past 16th")
            else Sast.Beat
    | SNote(pc, reg, b) ->
        let tpc = get_type program pc
        and treg = get_type program reg
        and tb = get_type program b in
        if tpc <> Sast.Int
        then type_error ("First element in Note (pitch class) must be of type Int " ^
            "between -1 and 11 but element was of type " ^ Sast.string_of_s_type tpc)
        else
            if treg <> Sast.Int
            then type_error ("Second element in Note (register) must be of type Int " ^
                "between 0 and 3 but element was of type " ^ Sast.string_of_s_type tpc)
            else
                if tb <> Sast.Int && tb <> Sast.Beat
                then type_error ("Third element in Note (Beat) must be of type Beat " ^
                    "but element was of type " ^ Sast.string_of_s_type tb)
                else Sast.Note
    | SList(el) -> (* Check all elements have same type*)
        (match el with 
          [] -> Sast.Empty
        | _ -> let hd = List.hd el in 
             let match_type_or_fail x y = 
                let tx = (get_type program x) in
                let ty = (get_type program y) in
                if diff_types [tx] [ty] && not (beats_and_ints tx ty) 
                    then type_error (string_of_sexpr x ^ " has type of "
                        ^ Sast.string_of_s_type tx ^ " but "
                        ^ string_of_sexpr y ^ " has type " 
                        ^ Sast.string_of_s_type ty ^ " in a same list")
                else () 
            in List.iter (match_type_or_fail hd) el; 
            if contains_beat el then powers_of_two el
            else Sast.List(get_type program (hd)))
    | SChord(el) -> (* Check all elements have type of TNote *)
        let hd = List.hd el in 
            let match_type_or_fail x y = 
                let tx = (get_type program x) in 
                let ty = (get_type program y) in 
                if tx <> ty 
                    then type_error ("Elements in Chord should all have type of " 
                    ^ Ast.string_of_types Ast.TNote ^ " but the element of " 
                    ^ string_of_sexpr y ^ " has type of " ^ Sast.string_of_s_type ty)
                else () in List.iter (match_type_or_fail hd) el; 
        let hd = List.hd el in 
            let match_duration_or_fail x y = match x, y with
                 SNote(p1,r1,bt1), SNote(p2,r2,bt2) -> 
                    (if (string_of_sexpr bt1) <> (string_of_sexpr bt2) 
                        then type_error ("The time durating of " ^ string_of_sexpr bt1
                        ^ " is not the consistent with that of " ^ string_of_sexpr bt2)
                        else ())
               | _,_ -> type_error ("Not Expected Exception")
        in List.iter (match_duration_or_fail hd) el; Sast.Chord
    | SSystem(el) -> (* Check all elements have type of TChord *)
        let hd = List.hd el in 
            let match_type_or_fail x y = 
                let tx = (get_type program x) in 
                let ty = (get_type program y) in 
                if tx <> ty 
                    then type_error ("Elements in Chord should all have type of " 
                    ^ string_of_s_type Sast.Chord ^ " but the element of " 
                    ^ string_of_sexpr y ^ " has type of " ^ string_of_s_type ty)
                else () in List.iter (match_type_or_fail hd) el; Sast.System
    | SLet(decs, exp) -> get_type program exp
    | SRandom -> Sast.Int
		| SPrint(e) -> get_type program e
		| SCall(f, args) -> 
			let poly_map = StringMap.empty in  
			let func_defs = find_f_def program f
				in (*print_string (string_of_s_func_decl func_def); *)
				let func_def = (match (List.hd func_defs) with 
						SFuncdec(x) -> x  | _ -> raise (Function_not_defined f)) in
				let f_var = find_var_entry program.symtab f in
				check_arg_types f program args f_var.v_type
			(* check all args against f type sig *)
			(* check expr matches last type *)
		| _ -> Sast.Still_unknown


and arg_has_type prog (a,t) = match a with 
	  SArglit(i)          -> t = Sast.Int
	| SArgbool(b)         -> t = Sast.Bool
	| SArgvar(v)          -> (get_type prog (SVariable(v))) = t
        | SArgbeat(e,i)       -> (get_type prog (SBeat(e,i))) = t
        | SArgnote(e1,e2,e3)  -> (get_type prog (SNote(e1,e2,e3))) = t
        | SArgchord(elist)    -> (get_type prog (SChord(elist))) = t
        | SArgsystem(elist)   -> (get_type prog (SSystem(elist))) = t
        | SArglist(elist)     -> (get_type prog (SList(elist))) = t
	| SArgparens(e)       -> (get_type prog e) = t

and check_arg_types name prog a_list t_list = 
	let ret_t = List.hd (List.rev t_list) in 
	if((List.length a_list) +1) <> (List.length t_list) then 
		raise (Wrong_number_of_arguments name)
	else let t_list = List.rev (List.tl (List.rev t_list)) in
		let tup = List.combine a_list t_list in 
			if not (List.fold_left (&&) true (List.map (arg_has_type prog) tup))
				then raise (Function_arguments_type_mismatch name)
			else ret_t


let rec not_list_of_unknowns = function
    Sast.Unknown -> false
    | Sast.List(x) -> not_list_of_unknowns x
    | _ -> true

let rec type_is_equal t1 t2 = 
	if( t1 = t2 ) then true
	else match t1 with 
		  Sast.List(a) -> (match t2 with
				  Sast.List(b) -> type_is_equal a b
				| Sast.Poly(b) -> true 
				| _ -> false )
		| Sast.Poly(a) -> true
		| _ -> (match t2 with 
					Sast.Poly(b) -> true
				| _ -> false )

let check_ret_type program types info = 
	(* Check that function value has correct type *)
    if not( type_is_equal (List.hd (List.rev types)) (get_type program info.s_value))
    then raise (Type_mismatch ("Expression of function " ^ info.s_fname ^
                    " " ^ String.concat " " (List.map string_of_patterns info.s_args)))	
		else program.symtab

let rec check_pat_types types info =
    
    (* Then make sure each pattern has correct type if not a var, and update the scope *)
    (*else*) let exp_pattypes = (List.rev (List.tl (List.rev types))) in
         let act_pattypes = (List.map get_pat_type info.s_args) in
         if List.mem true (List.map2 (fun epat apat -> apat <> Sast.Unknown &&
                                                       not_list_of_unknowns apat && epat <> apat)
                      exp_pattypes act_pattypes) then
            raise (Type_mismatch ("Patterns don't match type signature for " ^ info.s_fname ^ 
                    " " ^ String.concat " " (List.map string_of_patterns info.s_args)))
         else let pat_pairs = List.combine info.s_args exp_pattypes in
              let rec gen_scope = function
                  [] -> []
                 | (p, ty) :: rest ->
                    (match p, ty with
                     Patvar(s), _ -> {name = s; pats = []; v_type = [ty];
                                     v_expr = None} :: gen_scope rest
                    | Patcomma(l), Sast.List(lty) -> 
                        let tups = List.map (fun v -> (v, lty)) l in
                        (gen_scope tups) @ gen_scope rest
                    | Patcons(l1,l2), Sast.List(lty) ->
                        (gen_scope [(l1, lty)]) @ (match l2 with
                                                    | Patvar(s) -> [{name = s; pats = [];
                                                                    v_type = [ty];
                                                                    v_expr = None}]
                                                    | _ -> (gen_scope [(l2, ty)])) 
                        @ gen_scope rest
                    | _ -> gen_scope rest) in
         info.scope.identifiers <- gen_scope pat_pairs; info.scope

(* First pass walk_decl -> Try to construct a symbol table *)
let rec walk_decl prog = function
    Ast.Tysig(id,types) -> 
                let entry = {name=id; pats = []; v_type = (List.map types_to_s_type types); 
                            v_expr = None} in 
                if (exists_typesig id prog.symtab.identifiers)
                    then raise (Multiple_type_sigs id)
                else prog.symtab.identifiers <- mod_var entry prog.symtab; prog
    | Ast.Vardef(id, expr) -> 
                let var = {name=id; pats = []; v_type = [Unknown]; 
                          v_expr = Some(to_sexpr prog.symtab expr)} in
                if(exists_dec id "var" prog.decls) 
                    then raise (Multiple_declarations id)
                else prog.symtab.identifiers <- mod_var var prog.symtab;
                    { decls = SVardef(var, (to_sexpr prog.symtab expr)) :: prog.decls ;
                    symtab = prog.symtab} 
    | Ast.Funcdec(fdec) ->
            if (exists_dec fdec.fname "func" prog.decls)
                then raise (Multiple_declarations fdec.fname)
            else
                let f_vars = collect_pat_vars fdec.args in 
                let new_scope = {parent=Some(prog.symtab); identifiers = gen_new_scope f_vars} in
                let funcdef = SFuncdec({s_fname = fdec.fname; 
                                                type_sig = [Unknown];
                                                s_args = fdec.args;
                                                s_value = to_sexpr prog.symtab fdec.value;
                                                scope = new_scope;}) in 
                let var = {name = fdec.fname; pats = fdec.args;  v_type = [Unknown]; 
                           v_expr = Some(to_sexpr prog.symtab fdec.value)} in
                    prog.symtab.identifiers <- mod_var var prog.symtab;
                    { decls = funcdef :: prog.decls; symtab = prog.symtab }
    | Main(expr) -> 
        if(prog.symtab.parent = None) then 
            if( is_declared "main" prog.symtab) then raise (Multiple_declarations "main")
        else let mainvar = {name = "main"; pats = []; v_type = [Unknown]; v_expr = Some(to_sexpr prog.symtab expr)}
            in prog.symtab.identifiers <- (mod_var mainvar prog.symtab);
               { decls = (prog.decls @ [SMain(to_sexpr prog.symtab expr)]); symtab = prog.symtab }
         else raise Main_wrong_scope


(* Convert Ast expression nodes to Sast s_expr nodes (so we can have nested scopes) *)
and to_sexpr symbol = function
    | Ast.Literal(i) -> SLiteral(i)
    | Ast.Boolean(b) -> SBoolean(b)
    | Ast.Variable(s) -> SVariable(s)
    | Ast.Beat(e, i) -> SBeat(to_sexpr symbol e, i) 
    | Ast.Note(e1, e2, e3) -> SNote(to_sexpr symbol e1, to_sexpr symbol e2, to_sexpr symbol e3)
    | Ast.Binop(e1, op, e2) -> SBinop(to_sexpr symbol e1, op, to_sexpr symbol e2)
    | Ast.Prefix(pop, e) -> SPrefix(pop, to_sexpr symbol e)
    | Ast.If(e1,e2,e3) -> SIf(to_sexpr symbol e1, to_sexpr symbol e2, to_sexpr symbol e3)
    | Ast.List(elist) -> SList(List.map (fun s -> to_sexpr symbol s) elist)
    | Ast.Chord(elist) -> SChord(List.map (fun s -> to_sexpr symbol s) elist)
    | Ast.System(elist) -> SSystem(List.map (fun s -> to_sexpr symbol s) elist)
    | Ast.Call(e1, e2) -> SCall(e1, (List.map (fun s -> to_sarg symbol s)  e2))
    | Ast.Let(decs, e) -> let sym = {parent=Some(symbol); identifiers=[]} in                                
                             let nested_prog = List.fold_left walk_decl {decls=[]; symtab=sym} decs      
                             in let nested_prog2 = List.fold_left walk_decl_second nested_prog nested_prog.decls
                             in SLet(nested_prog2, to_sexpr sym e)

and to_sarg symbol = function
    | Ast.Arglit(i)           -> SArglit(i)
    | Ast.Argbool(b)          -> SArgbool(b)
    | Ast.Argvar(s)           -> SArgvar(s)
    | Ast.Argbeat(e, i)       -> SArgbeat(to_sexpr symbol e, i)
    | Ast.Argnote(e1, e2, e3) -> SArgnote(to_sexpr symbol e1, to_sexpr symbol e2, to_sexpr symbol e3)
    | Ast.Argchord(elist)     -> SArgchord(List.map (fun s -> to_sexpr symbol s) elist)
    | Ast.Argsystem(elist)    -> SArgsystem(List.map (fun s -> to_sexpr symbol s) elist)
    | Ast.Arglist(elist)      -> SArglist(List.map (fun s -> to_sexpr symbol s) elist)
    | Ast.Argparens(p)        -> SArgparens(to_sexpr symbol p)

(* Second pass -> use symbol table to resolve all semantic checks *)
and walk_decl_second program = function
    | SVardef(s_id, s_expr) as oldvar -> (*(print_string (string_of_s_program program));*)
        let texpr = [get_type program s_expr] in
        if (s_id.v_type = [Unknown]) then
            let new_type = if (exists_typesig s_id.name program.symtab.identifiers) then
                               let set_type = get_typesig s_id.name program.symtab.identifiers in
                               if diff_types set_type texpr then
                                    raise (Type_mismatch s_id.name)
                               else set_type
                           else texpr in 
            let newvar = SVardef({name = s_id.name; pats = []; v_type = new_type; 
                                  v_expr = s_id.v_expr}, s_expr) in
            replace_vardef program newvar oldvar
        else if diff_types s_id.v_type texpr then
            raise (Type_mismatch s_id.name)
        else program
    | SFuncdec(info) as oldfunc ->
        let types = get_types_p info.s_fname program.symtab in
        let argl = List.length info.s_args in
        let tyl = List.length types in
        if (argl <> tyl - 1) then raise (Pattern_num_mismatch( argl, tyl - 1))
        else let search_decls = List.filter (fun v -> v <> oldfunc) program.decls in
            if (List.length search_decls < (List.length program.decls) - 1) 
                || (same_pats info search_decls)
            then raise (Multiple_identical_pattern_lists 
                        (String.concat " " (List.map string_of_patterns info.s_args)))
            else 
							let n_prog = {decls = program.decls; symtab = (check_pat_types types info) } in 
							let newscope = check_ret_type n_prog types info in

                 let newfunc = SFuncdec({s_fname = info.s_fname; type_sig = types;
                                     s_args = info.s_args; s_value = info.s_value;
                                     scope = newscope;}) in
             replace_funcdec program newfunc oldfunc
  | SMain(expr) -> 
      let e_type = get_type program expr in 
				let new_main = {name = "main"; pats = []; v_type = [e_type]; v_expr = Some(expr)} in
				let program = replace_main program new_main in 
				(match e_type with
       Sast.Empty -> program
      | Sast.Note -> program
      | Sast.Chord | Sast.List(Sast.Note) -> program
      | Sast.System | Sast.List(Sast.Chord) | Sast.List(Sast.List(Sast.Note)) -> program
      | Sast.List(sys) -> if eventual_system sys then program else 
                          raise (Main_type_mismatch (string_of_sexpr expr))
      | _ -> raise (Main_type_mismatch (string_of_sexpr expr)))
    (*| _ -> program*)

let has_main program = 
  if(is_declared "main" program.symtab) then program
  else raise Main_missing

(* Right now gets called by smurf *)
let first_pass list_decs = 
    let program = List.fold_left walk_decl {decls=[]; symtab = global_env} list_decs
    in (*(print_string (string_of_s_program program));*) program

let second_pass list_decs = 
    let program = first_pass list_decs in 
        let real_program = List.fold_left walk_decl_second (has_main program) program.decls in
    (print_string (string_of_s_program real_program)); real_program.symtab
