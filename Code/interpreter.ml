(* File: interpeter.ml
 * interpret a file from AST to SMURFy code *)

open Ast
open Sast
open Util
open Printf
open Values
open Output
open Semanalyze

let ticks_16 = [| 1 |]
let ticks_8 = [| 2; 3 |]
let ticks_4 = [| 4; 6; 7 |]
let ticks_2 = [| 8; 12; 14; 15 |]
let ticks_1 = [| 16; 24; 28; 30; 31 |]

let r_max = 1000000

(* convernt the symbol table defined in Sast to environment defined in Values 
 * and set the parent of the new environment to the one passed to it 
 *)
(* Values.environment -> Sast.symbol_table -> Values.environment' *)
let st_to_env par st = 
    let newmap = List.fold_left (fun mp {v_expr=ve; name=nm; pats=pl} -> 
            NameMap.add nm {nm_expr=ve; nm_value=VUnknown} mp)
        NameMap.empty st.identifiers 
    in {parent=par; ids=newmap}


(* update a variable 'name' with the value of 'v',
 * looking for the definition of 'name' from the current
 * scope all the way up to the global scope, if can't find the
 * definition in the global scope, add a new binding of 
 * 'name' to 'v' in the current scope
 * function returns an updated environment chain
 *)
(* environment -> string -> value -> environment' *)
let rec update_env env name v = 
    match NameMap.mem name env.ids with
          true -> let newE = {parent=env.parent; 
              ids=NameMap.add name {nm_value=v;nm_expr=None} env.ids} in (*show_env newE;*) newE
        | false -> match env.parent with
              None -> let newE = {parent=env.parent; 
                  ids=NameMap.add name {nm_value=v;nm_expr=None} env.ids} in (*show_env newE;*) newE
            | Some par -> let newE  = {parent=Some (update_env par name v); 
                ids=env.ids} in (*show_env newE;*) newE


(* searching for the definition of a name, returns its value *)
(* environment -> string -> value,environment' *)
let rec resolve_name env symtab name =
    match NameMap.mem name env.ids with
          true -> let id=(NameMap.find name env.ids) in (*print_string ("In resolve_name we found the name " ^ name);*)
            (match id.nm_expr with
                  Some expr -> (*print_string ("We found an expr for " ^name ^ "and it is: " ^(string_of_sexpr expr) ^ "\n");*)let (v,env1)=(eval env symtab expr) in 
                    let env2 = update_env env1 name v in v,env2
                | None -> (*print_string ("No expr for " ^ name ^ " but we have a value of: " ^ (string_of_value id.nm_value) ^ "\n");*) id.nm_value,env)
        | false -> match env.parent with
              None -> interp_error ("Can't find binding to " ^ name)
            | Some par -> resolve_name par symtab name

(* eval : env -> Sast.expression -> (value, env') *)
(* evaluate the expression, return the result and the updated 
 * environment, the environment updated includes the 
 * current and all the outer environments that modified
 *)
(* environment -> symbol_table -> Sast.s_expr -> (value, environment') *)
and eval env symtab = function
      Sast.SLiteral(x) -> (VInt(x), env)
    | Sast.SBoolean(x) -> (VBool(x), env)
    | Sast.SVariable(str) -> 
        let v,env' = resolve_name env symtab str in v,env'
    | Sast.SBeat(e, n) ->  if n < 0 then interp_error ("Somehow we have a negative number of dots on a beat!")
        else let (ve,env1) = eval env symtab e in
        (match ve with
            | VInt(x) -> (match x with
                1 -> if n > 4 then interp_error ("A whole Beat may only have up to 4 dots")
                     else (VBeat(ticks_1.(n)),env1)
              | 2 -> if n > 3 then interp_error ("A half Beat may only have up to 3 dots")
                     else (VBeat(ticks_2.(n)),env1)
              | 4 -> if n > 2 then interp_error ("A quarter Beat may only have up to 2 dots")
                     else (VBeat(ticks_4.(n)),env1)
              | 8 -> if n > 1 then interp_error ("An 8th Beat may only have up to 1 dot")
                     else (VBeat(ticks_8.(n)),env1)
              | 16 -> if n > 0 then interp_error ("A 16th Beat may not have dots")
                      else (VBeat(ticks_16.(n)),env1)
              | _ -> interp_error ("Beat must be a power of 2 between 1 & 16"))

            | _ -> interp_error ("Not expected Beat values"))
    | Sast.SNote(pc, reg, beat) ->(*print_string "HERE WE GO";*)
        (let (vpc,env1) = eval env symtab pc in
         let (vreg,env2) = eval env1 symtab reg in
         let (vbeat,env3) = eval env2 symtab beat in let vbeat =
          (match vbeat with
           VBeat(_) -> vbeat
           | VInt(x) -> if List.mem x [1;2;4;8;16] then VBeat(16/x) else interp_error ("Non-power of two being used as a beat")
           | _ -> interp_error ("We have a note with a non-int non-beat Beat value")) in (*print_string ("Making a note with number of ticks " ^ (string_of_value vbeat));*) VNote(vpc,vreg,vbeat),env3)
    | Sast.SBinop(e1, op, e2) -> (*Incomplete*)
        (let (v1,env1) = eval env symtab e1 in
         let (v2,env2) = eval env1 symtab e2 in
         let ticks = [| 0; 16; 8; 0; 4; 1; 0; 0; 2; 0; 0; 0; 0; 0; 0; 0; 1|] in
         (match v1, v2 with
            | VInt(x), VList(lst) ->
                (match op with
                    Trans -> if not (List.for_all (fun v -> match v with VInt(x) ->
                                            if x >= 0 || x <= 11 then true else false
                                            | _ -> false) lst)
                             then interp_error ("Non pitch class integer found in inversion list")
                             else VList(List.map (fun v -> match v with VInt(y) ->
                                            VInt((x+y) mod 12)
                                            | _ -> interp_error ("Ran into a transposition error"))
                                            lst), env2
                   | Cons -> (match (List.hd lst) with
                       VInt(_) -> VList(v1 :: lst), env2
                       |_ -> interp_error ("Trying to cons an int onto a list of non-ints"))
                   | _ -> (*print_string ("Problem expression: " ^ (string_of_sexpr (Sast.SBinop(e1,op,e2))) ^ "\n");*) interp_error ("The only op that can be used between an int
                        and a list is the transposition operator"))
            | VInt(x), VInt(y) ->
                (match op with
                      Add -> VInt(x+y),env2
                    | Sub -> VInt(x-y),env2
                    | Mul -> VInt(x*y),env2
                    | Div -> 
                        if y<>0
                        then VInt(x/y),env2
                        else interp_error ("Cannot divide by zero")
                    | Mod ->
                        if y<0
                        then VInt(x mod (y*(-1))),env2
                        else VInt(x mod y),env2
                    | PCAdd -> VInt((x+y) mod 12),env2
                    | PCSub -> VInt((x-y) mod 12),env2
                    | Less -> VBool(x<y),env2
                    | Leq -> VBool(x<=y),env2
                    | Greater -> VBool(x>y),env2
                    | Geq -> VBool(x>=y),env2
                    | BoolEq -> VBool(x=y),env2
                    | BeatAdd ->
                        if List.mem x [1;2;4;8;16] && List.mem y [1;2;4;8;16]
                        then (* This is a hacky way of doing this *)
                            VBeat(ticks.(x) + ticks.(y)),env2
                        else interp_error ("Ints used in Beat operation must be power of 2 "
                            ^ "between 1 & 16")
                    | BeatSub ->
                        if List.mem x [1;2;4;8;16] && List.mem y [1;2;4;8;16]
                        then (* This is a hacky way of doing this *)
                            if ticks.(x) > ticks.(y)
                            then VBeat(ticks.(x) - ticks.(y)),env2
                            else interp_error ("First operand must be greater than second in Beat subtraction")
                        else interp_error ("Ints used in Beat operation must be power of 2 "
                            ^ "between 1 & 16")
                    | BeatMul ->
                        if y>0 then
                            if List.mem x [1;2;4;8;16]
                            then (* This is a hacky way of doing this *)
                                VBeat(ticks.(x) * y),env2
                            else interp_error ("Ints used in Beat operation must be power of 2 "
                                ^ "between 1 & 16")
                        else interp_error ("Must multiple Beat by positive Int")
                    | BeatDiv ->
                        if y>0 then
                            if List.mem x [1;2;4;8;16]
                            then (* This is a hacky way of doing this *)
                                if ticks.(x) > y
                                then VBeat(ticks.(x) / y),env2
                                else interp_error ("First operand must be greater than second in Beat division")
                            else interp_error ("Ints used in Beat operation must be power of 2 "
                                ^ "between 1 & 16")
                        else interp_error ("Must divide Beat by positive Int")
                    | BeatLess ->
                        if List.mem x [1;2;4;8;16] && List.mem y [1;2;4;8;16]
                        then (* This is a hacky way of doing this *)
                            VBool(ticks.(x) < ticks.(y)),env2
                        else interp_error ("Ints used in Beat operation must be power of 2 "
                            ^ "between 1 & 16")
                    | BeatLeq ->
                        if List.mem x [1;2;4;8;16] && List.mem y [1;2;4;8;16]
                        then (* This is a hacky way of doing this *)
                            VBool(ticks.(x) <= ticks.(y)),env2
                        else interp_error ("Ints used in Beat operation must be power of 2 "
                            ^ "between 1 & 16")
                    | BeatGreater ->
                        if List.mem x [1;2;4;8;16] && List.mem y [1;2;4;8;16]
                        then (* This is a hacky way of doing this *)
                            VBool(ticks.(x) > ticks.(y)),env2
                        else interp_error ("Ints used in Beat operation must be power of 2 "
                            ^ "between 1 & 16")
                    | BeatGeq ->
                        if List.mem x [1;2;4;8;16] && List.mem y [1;2;4;8;16]
                        then (* This is a hacky way of doing this *)
                            VBool(ticks.(x) >= ticks.(y)),env2
                        else interp_error ("Ints used in Beat operation must be power of 2 "
                            ^ "between 1 & 16")
                    | _ -> interp_error ("Not expected op for Ints"))
            | VBeat(x), VBeat(y) ->
                (* Operations act the same as normal because Beat has been converted to Ticks*)
                (match op with
                      BeatAdd -> VBeat(x+y),env2
                    | BeatSub ->
                        if x > y
                        then VBeat(x-y),env2
                        else interp_error ("First operand must be greater than second in Beat subtraction")
                    | BeatLess -> VBool(x<y),env2
                    | BeatLeq -> VBool(x<=y),env2
                    | BeatGreater -> VBool(x>y),env2
                    | BeatGeq -> VBool(x>=y),env2
                    | _ -> interp_error ("Not expected op for Beats"))
            | VBeat(x), VInt(y) -> 
                (match op with
                    | BeatAdd ->
                        if List.mem y [1;2;4;8;16]
                        then VBeat(x + ticks.(y)),env2
                        else interp_error ("Ints used in Beat operation must be power of 2 " ^
                            "between 1 & 16")
                    | BeatSub ->
                        if List.mem y [1;2;4;8;16] then
                            if x > ticks.(y)
                            then VBeat(x - ticks.(y)),env2
                            else interp_error ("First operand must be greater than second in Beat subtraction")
                        else interp_error ("Ints used in Beat operation must be power of 2 " ^
                            "between 1 & 16")
                    | BeatMul ->
                        if y>0
                        then VBeat(x*y),env2
                        else interp_error ("Must multiple Beat by positive Int")
                    | BeatDiv ->
                        if y>0 then
                            if x>y
                            then VBeat(x/y),env2
                            else interp_error ("First operand must be greater than second in Beat division")
                        else interp_error ("Must divide Beat by positive Int")
                    | BeatLess ->VBool(x < ticks.(y)),env2
                    | BeatLeq ->VBool(x <= ticks.(y)),env2
                    | BeatGreater -> VBool(x > ticks.(y)),env2
                    | BeatGeq -> VBool(x >= ticks.(y)),env2
                    | _ -> interp_error ("Not expected op for Beats"))
            | VInt(x), VBeat(y) -> if not (List.mem x [1;2;4;8;16]) then 
                                   interp_error ("Ints used in Beat operation must be power of 2 " ^ "between 1 & 16")
                                   else
                (match op with
                    | BeatAdd -> VBeat(ticks.(x) + y),env2
                    | BeatSub -> if ticks.(x) > y
                                then VBeat(ticks.(x) - y),env2
                                else interp_error ("First operand must be greater than second in Beat subtraction")
                    | BeatLess -> VBool(ticks.(x) < y),env2
                    | BeatLeq -> VBool(ticks.(x) <= y),env2
                    | BeatGreater -> VBool(ticks.(x) > y),env2
                    | BeatGeq -> VBool(ticks.(x) >= y),env2
                    | _ -> interp_error ("Not expected op for Beats"))
            | VBool(x), VBool(y) ->
                (match op with
                      And -> VBool(x && y),env2
                    | Or -> VBool(x || y),env2
                    | _ -> interp_error ("Not expected op for Bools"))
            | VList([]), x -> 
                (match x with
                     VList(m) -> 
                        (match op with
                            Concat -> VList(m),env2
                           | Cons -> (match m with 
                                      [VSystem(sys)] -> 
                                        VList([VSystem(VChord([VNote(VInt(-1),VInt(-1),VBeat(-1))])::sys)]),env2
                                      | _ -> VList(VList([])::m),env2)
                           | _ -> interp_error ("Not expected op between empty list and List"))
                     | VChord(m) -> (match op with
                            Concat -> VChord(m),env2
                            | Cons -> VChord((VNote(VInt(-1),VInt(-1),VBeat(-1))) :: m),env2
                            | _ -> interp_error ("Not expected op between empty list and Chord"))
                     | VSystem(n) -> (match op with
                             Concat -> (VSystem(n),env2)
                           | Cons -> (VSystem(VChord([VNote(VInt(-1),VInt(-1),VBeat(-1))])::n),env2)
                           | _ -> interp_error ("Not expected op between empty list and System"))
                     |_ -> interp_error("Empty list being applied to nonlist operand in binary operation"))
            | x, VList([]) ->
                (match op with
                    Concat -> x, env2
                  | Cons -> VList([x]), env2
                  | _ -> interp_error ("Not expected op given two lists with second being the empty list"))
            | VList(lx), VList(ly) -> 
                (match op with
                      Concat -> VList(lx @ ly),env2
                    | Cons -> (match (List.hd ly) with
                                VList(_) -> VList(v1 :: ly),env2
                               |VChord(_) -> (match (List.hd lx) with
                                               VNote(_,_,_) -> VList(v1 :: ly)
                                             | _ -> interp_error ("Cannot cons non-note " ^ (string_of_value v1) ^ " onto chord")),env2
                               | _ -> interp_error ("Cannot cons " ^ (string_of_value v1) ^ " onto " ^ (string_of_value v2)))
                    | _ -> interp_error ("Not expected op for Lists: " ^ (string_of_value v1) ^ " " ^ (string_of_value v2)))
            | VNote(a,b,c), (VList(lst) | VChord(lst)) -> (match op with
                    Cons -> let notetester = (fun note-> match note with VNote(d,e,f) -> f =c | _ -> false) in
                            (match (List.hd lst) with
                                VNote(_,_,_) -> (match v2 with
                                                    VChord(_) -> if List.for_all notetester lst then VChord(v1 :: lst)
                                                                else interp_error ("One of the notes in " ^ (string_of_value v2) ^
                                                                    " does not have the same duration as " ^ (string_of_value v1))

                                                   | _ -> VList(v1 :: lst)), env2
                              | _ -> interp_error ("Cannot cons a note to a list of non-notes"))
                  | _ -> interp_error ("Not expected op given a note and a list"))
            | VChord(a), VList(lst)-> (match op with
                    Cons -> (match (List.hd lst) with
                        VChord(_) -> VList(v1 :: lst), env2
                      | VList(VNote(_,_,_) :: _) -> VList(v1 :: lst), env2
                      | _ -> interp_error ("Cannot cons a chord to a list of non-chords"))
                   | _ -> interp_error ("Note expected op given a chord and a list"))
            | VChord(a), VSystem(lst) -> (match op with
                    Cons -> VSystem(v1 :: lst), env2
                   | _ -> interp_error ("Note expected op given a chord and a system"))
            | x, y ->
                (match op with
                      BoolEq -> VBool(x=y),env2
                    | _ -> interp_error ((string_of_value x) ^ " " ^(string_of_value y) ^ ": Not expected operands")))
         )
    | Sast.SPrefix(op, e) -> (*Incomplete*)
        (let (v1,env1) = eval env symtab e in
         match v1 with
            | VBool(x) -> (match op with
                | Not -> VBool(not x),env1
                | _ -> interp_error ("Unexpected op for Bool"))
            | VList(lst) -> (match op with
                | Retro -> VList(List.rev lst),env1
                | Inv -> if List.for_all (fun v -> match v with VInt(x) ->
                                    if x >= 0 || x <= 11 then true 
                                    else interp_error ("Non pitch class integer found in inversion list")
                                   | _ -> false) lst then
                         let row = List.map (fun v -> match v with 
                                VInt(x) -> x 
                              | _ -> interp_error("Non int found in a list of int")) lst in
                         let base = List.hd row in
                         let transrow = List.map (fun v -> v - base) row in
                         let invrow = List.map (fun v -> 12 - v) transrow in
                         let finalrow = List.map (fun v -> v + base) invrow in 
                         VList(List.map (fun v -> VInt(v)) finalrow), env1
                         else interp_error ("Inversion called on non-tone row")

                | _ -> interp_error ("Unexpected op for list"))
            | _ -> interp_error ("Unexpected operand for prefix op")
         )
    | Sast.SIf(e1, e2, e3) -> 
        (match eval env symtab e1 with
            | VBool(true), env -> eval env symtab e2 
            | VBool(false), env -> eval env symtab e3
            | _ -> interp_error ("error in If expr"))
    | Sast.SList(el) -> if el = [] then  (VList([]), env) else  (*updating evironment after eval every expression*)
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env symtab e in (env',v::lst)) 
                (env,[]) el) in VList(List.rev lst), env')
    | Sast.SChord(el) -> 
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env symtab e in (env',v::lst)) 
                (env,[]) el) in VChord(List.rev lst), env')
    | Sast.SSystem(el) -> 
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env symtab e in (env',v::lst))
                (env,[]) el) in VSystem(List.rev lst), env')

    | Sast.SCall(e1, e2) -> 
        let sid_lst = List.find_all (fun f -> f.name = e1) symtab.identifiers in
        let sid = 
        try 
        List.find (fun sid -> let flag,_ = bind_pat_arg env symtab sid.pats e2 in flag) sid_lst 
        with Type_error x -> type_error x
            | Not_found -> interp_error ("Matched patern not found!")
        in 
        let flag,newE = bind_pat_arg env symtab sid.pats e2 in 
        (match sid.v_expr with
          Some(e) -> (*print_string ("The expression we ended up with is: "^(string_of_sexpr e)^"\n"); *)
                    (match (eval newE symtab e) with v, _ -> v,env )
        | None -> (*print_string "WE GOT NONE\n"; *)interp_error ("Function declaration without expression"))

    | Sast.SLet(s_prog,e) -> (* reutrn the original env *)
        let local_env = st_to_env (Some env) s_prog.symtab in 
        let local_env1 = List.fold_left exec_decl local_env s_prog.decls in
        show_env local_env1; let v,local_env2 = (eval local_env1 symtab e) in v,env
    | Sast.SRandom -> Random.self_init (); (VInt(Random.int r_max), env)
    | Sast.SPrint(e1) -> (*print_string ("\n"^(string_of_value (fst (eval env symtab e1)))^"\n") ;*) eval env symtab e1 


(* environment -> pattern list -> arg list -> (Bool,environment') *)
and bind_pat_arg env symtab patl argl = 
    if (List.length patl) <> (List.length argl) then 
         type_error ("number of arguments does not match number of patterns")
    else
    (*print_string (String.concat " " (List.map string_of_patterns patl));
    print_string ("\n" ^ String.concat " and " (List.map string_of_sfargs (List.rev argl)) ^ "\n");*)
    let combl = List.combine patl (List.rev argl) in
    let flag,nmp = List.fold_left (fun (flag,mp) (p,a) -> let b,mp' = is_pat_arg_matching env symtab p a mp in (*print_string (string_of_bool b); *)
            (flag&&b,mp')) (true,NameMap.empty) combl 
    in (*print_string "RETURNING FROM BIND_PAT\n";*) flag,{parent=Some(env); ids=nmp}

and gen = function
    _ as v -> {nm_expr = None; nm_value=v}

(* pattern -> value -> NameMap -> (Bool, NameMap') *)
and is_pat_val_matching env symtab pat value mp =
    match pat with 
          Patconst(pi) -> (match value with 
                          VInt(ai) -> if pi = ai then true,(mp) else false,mp
                        | _ -> false,(mp))
        | Patbool(pb) -> (match value with 
                          VBool(ab) -> if pb = ab then true,(mp) else false,mp
                        | _ -> false,mp)
        | Patvar(ps) -> (*print_string "IN PAT_VAL MATCHING\n\n\n";*)(match value with 
                          VInt(ai) -> true,(NameMap.add ps (gen (VInt(ai))) mp)
                        | VBool(ab) -> true,(NameMap.add ps (gen (VBool(ab))) mp)
                        | VBeat(i) -> true, (NameMap.add ps (gen value) mp)
                        | VNote(_,_,_) | VChord(_) | VSystem(_) | VList(_) -> true,(NameMap.add ps (gen value) mp)
                        | _ -> interp_error ("We have an unknown value in the interpreter...\n"))
        | Patwild -> true,(mp)
        | Patcomma(pl) -> (match value with 
                        | VList(al) | VChord(al) | VSystem(al) -> (if List.length pl <> List.length al then 
                            false,mp 
                            else
                            let lst = List.combine pl al in 
                            List.fold_left (fun (b,m) (p,a) -> let r1,r2 = match_pat_value env symtab p a m in (b&&r1),r2) (true,mp) lst)
                        | _ ->  false,mp)
        | Patcons(p1,p2) -> (match value with
                        | VList(al) -> (if List.length al = 0 then 
                            false,mp 
                            else
                            (match al with 
                              h::tl -> 
                                (let r1,r2 = match_pat_value env symtab p1 h mp in
                                 let r3,r4 = is_pat_val_matching env symtab p2 (VList(tl)) r2 in (r1&&r3),r4)
                            | _ -> false,mp))
                        | _ -> false,mp)

(* pattern -> argument -> NameMap -> (Bool,NameMap') *)
and is_pat_arg_matching env symtab pat arg mp = 
    match pat with 
          Patconst(pi) -> (match arg with 
                          SArglit(ai) -> if pi = ai then true,(mp) else false,mp
                        |  SArgparens(expr) -> (let v,_ = eval env symtab expr in 
                                                (match v with 
                                                 VInt(ai) -> if pi = ai then true, (mp) else false,mp
                                                | _ -> false,mp))
                        | SArgvar(id) -> let v,_ = resolve_name env symtab id in if (match v with
                                                    VInt(ai) -> pi = ai
                                                   | _ -> false) then true, (mp) else false,mp
                        | _ -> false,(mp))
        | Patbool(pb) -> (match arg with 
                          SArgbool(ab) -> if pb = ab then true,(mp) else false,mp
                        | _ -> false,mp)
        | Patvar(ps) -> (*print_string "Patvar\n"; *)(match arg with 
                          SArglit(ai) -> true,(NameMap.add ps (gen (VInt(ai))) mp)
                        | SArgbool(ab) -> true,(NameMap.add ps (gen (VBool(ab))) mp)
                        | SArgvar(str) -> (*print_string ("In is_pat_arg_matching we're trying to match pattern " ^ (string_of_patterns (Patvar(ps))) ^ 
                                                        " with argument " ^ str);*) let v,_ = resolve_name env symtab str in true,(NameMap.add ps (gen v) mp)
                        | SArgbeat(e,i) -> 
                            (match (eval env symtab (SBeat(e,i))) with
                               (VBeat(aa),_) -> true,(NameMap.add ps (gen (VBeat(aa))) mp)
                             | _ -> false,(mp))
                        | SArgnote(p,r,b) -> 
                            (match eval env symtab (SNote(p,r,b)) with
                               VNote(v1,v2,v3),_ -> true,(NameMap.add ps (gen (VNote(v1,v2,v3))) mp)
                             | _ -> false,mp)
                        | SArgchord(el) -> 
                            (let vl,env = List.fold_left (fun (l,env) e -> let res,env' = eval env symtab e in (res::l),env') ([],env) el in
                             true,(NameMap.add ps (gen (VChord(vl))) mp))
                        | SArgsystem(el) -> 
                            (let vl,env = List.fold_left (fun (l,env) e -> let res,env' = eval env symtab e in (res::l),env') ([],env) el in
                             true,(NameMap.add ps (gen (VSystem(vl))) mp))
                        | SArglist(el) -> 
                            (let vl,env = List.fold_left (fun (l,env) e -> let res,env' = eval env symtab e in (res::l),env') ([],env) el in
                             true,(NameMap.add ps (gen (VList(vl))) mp))
                        | SArgparens(expr) -> (*print_string ("Dealing with parens expr: " ^ (string_of_sexpr expr) ^ "\n");*)
                            (let v,_ = eval env symtab expr in true,(NameMap.add ps (gen v) mp)))
        | Patwild -> true,(mp)
        | Patcomma(pl) -> (*print_string "Patcomma\n";*)(match arg with 
                        | SArglist(al) -> (if List.length pl <> List.length al then 
                            false,mp 
                            else
                            let lst = List.combine pl al in 
                            List.fold_left (fun (b,m) (p,a) -> let r1,r2 = match_pat_expr env symtab p a m in (b&&r1),r2) (true,mp) lst)
                        | SArgvar(id) -> (*print_string ("For patcomma, we have an argument var named " ^ id ^ "\n");*)
                                         let v,_ = resolve_name env symtab id in (match v with
                                             VList(lst) -> (match lst with
                                                           | [] -> if pl = [] then true,(mp) else false,(mp)
                                                           | hd::tl -> if pl = [] then false,(mp) else
                                                                (let r1,r2 = match_pat_value env symtab (List.hd pl) hd mp in
                                                                 let r3,r4 = is_pat_val_matching env symtab (Patcomma((List.tl pl))) (VList(tl)) r2 in (r1 &&r3),r4))
                                           | _ -> interp_error ("Not working right now"))
                        | _ -> false,mp)
        | Patcons(p1,p2) -> (match arg with
                        | SArglist(al) | SArgchord(al) | SArgsystem(al)-> (if List.length al = 0 then 
                            false,mp 
                            else
                            (match al with 
                              h::tl -> 
                                (let r1,r2 = match_pat_expr env symtab p1 h mp in
                                 let r3,r4 = is_pat_arg_matching env symtab p2 
                                 (match arg with
                                  SArglist(_) -> SArglist(tl)
                                 |SArgchord(_) -> SArgchord(tl)
                                 |SArgsystem(_) -> SArgsystem(tl)
                                 | _ -> interp_error("Not acceptable")) r2 in (r1&&r3),r4)
                            | _ -> false,mp))
                        | SArgvar(id) -> (*print_string ("For patcons, we have an argument var named " ^ id  ^ "\n");*)
                                         let v,_ = resolve_name env symtab id in (match v with
                                             VList(lst) | VChord(lst) | VSystem(lst) -> (match lst with
                                                            h::tl ->
                                                                (let r1,r2 = match_pat_value env symtab p1 h mp in
                                                                 let r3,r4 = is_pat_val_matching env symtab p2 
                                                                 (match v with
                                                                  VList(_) -> VList(tl)
                                                                 |VChord(_) -> VChord(tl)
                                                                 |VSystem(_) -> VSystem(tl)
                                                                 | _-> interp_error("Not acceptable"))
                                                                  r2 in (r1 &&r3),r4)
                                                           | _ -> false, mp)
                                             | _ ->  false, mp)
                        | SArgparens(exp) -> let r1,r2 =match_pat_expr env symtab pat exp mp in r1,r2 
                        | _ -> false,mp)
    (*
        *)


and match_pat_expr env symtab pat expr mp = 
    let arg,env = eval env symtab expr in (*print_string (string_of_value arg);*)
    match pat with
          Patconst(pi) -> (match arg with 
                          VInt(ai) -> if pi = ai then true,(mp) else false,mp
                        | _ -> false,(mp))
        | Patbool(pb) -> (match arg with 
                          VBool(ab) -> if pb = ab then true,(mp) else false,mp
                        | _ -> false,mp)
        | Patvar(ps) -> true,(NameMap.add ps (gen arg) mp)
        | Patwild -> true,(mp)
        | Patcons(p1,p2) -> (match arg with VList(lst) -> (match lst with
                                                            h::tl ->
                                                                (let r1,r2 = match_pat_value env symtab p1 h mp in
                                                                 let r3,r4 = is_pat_val_matching env symtab p2 (VList(tl)) r2 in (r1 &&r3),r4)
                                                           | _ -> false, mp)
                                             | _ ->  false, mp)

        | _ -> false,mp
    
        
(* same as match_pat_expr but matches pattern against value, which occurs when we're comparing
    a list pattern with a variable argument in is_pat_arg_matching *)
and match_pat_value env symtab pat value mp =
    match pat with
      Patconst(pi) -> (match value with 
                      VInt(ai) -> if pi = ai then true,(mp) else false,mp
                    | _ -> false,(mp))
    | Patbool(pb) -> (match value with 
                      VBool(ab) -> if pb = ab then true,(mp) else false,mp
                    | _ -> false,mp)
    | Patvar(ps) -> (*print_string "TRUE!";*)true,(NameMap.add ps (gen value) mp)
    | Patwild -> true,(mp)
    | _ -> false,mp

        

(* exec_decl : env -> decl -> env' *)
(* execute the top-level declaration, in the global enviroment, 
 * return the updated global environment. Seems several decls needn't 
 * be execed as we only evaluate the dependencies of main *)
(* environment -> Sast.s_dec -> environment' *)
and exec_decl env = function
    (*
      Sast.STypesig(sid) -> (* signature will generate a new fun *)
        (let vfun = VFun(sid.name,s_id,[]) in update_env env str vfun)
    | Sast.SFuncdec(f_decl) -> (* fun decl will be added to current *)
        (match NameMap.mem f_decl.fname env.ids with
              true -> (match (NameMap.find f_decl.fname env.ids) with 
                        | {nm_value=VFun(name,fsig,def)} -> 
                            let vfun = VFun(name, fsig, f_decl::def) in update_env env name vfun
                        | _ -> interp_error("Not defined as a signature"))
            | false -> interp_error ("Function definition without a signature"))
    | Sast.SVardef(sid,se) -> 
        let v,env' = eval env se in
            update_env env' sid.name v
    | Sast.SMain(e) -> 
        (let v, env' = eval env e in 
            write_to_file bytecode_name v; update_env env' "main" v)
    *)
    | _ -> trace ("Unsupported!") env


(* The entry of evaluation of the program *)
(* environment -> configuration -> unit *)
let exec_main symtab config = 
    let globalE=(st_to_env None symtab) in 
    let main_entry =NameMap.find "main" globalE.ids in
    let main_expr = (match main_entry.nm_expr with 
              None -> interp_error "main has no definition!"
            | Some expr -> expr) in
    let v, env' = eval globalE symtab main_expr in 
    let _ = write_to_file config.bytecode_name v; update_env env' "main" v in
    let cmd = ("java -jar " ^ config.lib_path ^ " " ^ config.bytecode_name ^ " " ^ config.midi_name) in
    print_string (cmd ^ "\n"); 
    let result_code = Sys.command cmd
    in (match result_code with 
          0 -> 
            print_string ("===== Program Successfully Finished =====\n");
            print_string ("===== Result Writen to " ^ config.midi_name ^ " =====\n")
        | _ as error_code -> print_string ("Error: *** Program Terminates With Code " ^ string_of_int error_code ^ "\n")
        )


(* run : program -> () *)
(* run the program. original one, depreciated *)
let run program s_prog = 

let decls = program in 
let globalE = {parent = None; 
        ids = List.fold_left (fun mp lst -> 
        NameMap.add lst.name {nm_value=VUnknown; nm_expr=None} mp) 
        NameMap.empty s_prog.symtab.identifiers}
in let _ = show_env globalE in

(* top-level declarations always run in global environment *)
List.fold_left exec_decl globalE decls

