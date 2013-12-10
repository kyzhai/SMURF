(* File: interpeter.ml
 * interpret a file from AST to SMURFy code *)

open Ast
open Sast
open Util
open Printf
open Values
open Output
    
(* convernt the symbol table defined in Sast to environment defined in Values 
 * and set the parent of the new environment to the one passed to it 
 *)
(* Values.environment -> Sast.symbol_table -> Values.environment' *)
let st_to_env par st = 
    let newmap = List.fold_left (fun mp {name=nm; v_expr=ve} -> 
            NameMap.add nm {v_expr=ve; v_value=VUnknown} mp) 
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
              ids=NameMap.add name {v_value=v;v_expr=None} env.ids} in show_env newE; newE
        | false -> match env.parent with
              None -> let newE = {parent=env.parent; 
                  ids=NameMap.add name {v_value=v;v_expr=None} env.ids} in show_env newE; newE
            | Some par -> let newE  = {parent=Some (update_env par name v); 
                ids=env.ids} in show_env newE; newE


(* searching for the definition of a name, returns its value *)
(* environment -> string -> value *)
let rec resolve_name env name =
    match NameMap.mem name env.ids with
          true -> let id=(NameMap.find name env.ids) in 
                    (match id.v_expr with
                          Some expr -> let (v,env')=(eval env expr) in v
                        | None -> id.v_value)
        | false -> match env.parent with
              None -> interp_error ("Can't find binding to " ^ name)
            | Some par -> resolve_name par name
(* eval : env -> Sast.expression -> (value, env') *)
(* evaluate the expression, return the result and the updated 
 * environment, the environment updated includes the 
 * current and all the outer environments that modified
 *)
(* environment -> Sast.s_expr -> (value, environment') *)
and eval env = function
      Sast.SLiteral(x) -> (VInt(x), env)
    | Sast.SBoolean(x) -> (VBool(x), env)
    | Sast.SVariable(str) -> (resolve_name env str, env)
    | Sast.SBeat(e, n) -> trace ("eval beat: ") 
        (let (v,env') = eval env e in VBeat(v,n), env')
    | Sast.SNote(e1, e2, e3) -> trace ("eval note: ") 
        (let (v1,env1) = eval env e1 in 
         let (v2,env2) = eval env1 e2 in 
         let (v3,env3) = eval env2 e3 in VNote(v1,v2,v3),env3)
    | Sast.SBinop(e1, op, e2) -> (*Incomplete*)
        (let (v1,env1) = eval env e1 in 
         let (v2,env2) = eval env1 e2 in 
         (match v1, v2 with 
            | VInt(x), VInt(y) -> 
                (match op with 
                      Add -> VInt(x+y),env2
                    | Sub -> VInt(x-y),env2
                    | Mul -> VInt(x*y),env2
                    | Div -> VInt(x/y),env2
                    | Mod -> VInt(x mod y),env2
                    | _ -> interp_error ("Not expected op for Ints"))
            | VList(lx), VList(ly) -> 
                (match op with
                      Concat -> VList(lx @ ly),env2
                    | _ -> interp_error ("Not expected op for Lists"))
            | _ -> interp_error ("Not expected operands")
         ))
    | Sast.SPrefix(op, e) -> (*Incomplete*)
        (let (v1,env1) = eval env e in
         match v1 with 
            | VList(lst) -> (match op with
                | Retro -> VList(List.rev lst),env1
                | _ -> interp_error ("Unexpected op for list"))
            | VChord(lst) -> (match op with
                | Retro -> VChord(List.rev lst),env1
                | _ -> interp_error ("Unexpected op for chord"))
            | VSystem(lst) -> (match op with
                | Retro -> VSystem(List.rev lst),env1
                | _ -> interp_error ("Unexpected op for system"))
            | _ -> interp_error ("Unexpected operand for prefix op")
         )
    | Sast.SIf(e1, e2, e3) -> 
        trace "eval if: " (match eval env e1 with
            | VBool(true), env -> trace "true branch" eval env e2 
            | VBool(false), env -> trace "false branch" eval env e3
            | _ -> interp_error ("error in If expr"))
    | Sast.SList(el) -> (*updating evironment after eval every expression*)
        trace "eval list: " 
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env e in (env',v::lst)) 
                (env,[]) el) in VList(List.rev lst), env')
    | Sast.SChord(el) -> 
        trace "eval Chord: " 
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env e in (env',v::lst)) 
                (env,[]) el) in VChord(List.rev lst), env')
    | Sast.SSystem(el) -> 
        trace "eval System: " 
        (let (env',lst)=(List.fold_left (fun (env,lst) e -> 
                    let v, env' = eval env e in (env',v::lst)) 
                (env,[]) el) in VSystem(List.rev lst), env')
    | Sast.SCall(e1, e2) -> trace "eval Call: Why the function call only takes one parementer?" (VUnknown, env)
    | Sast.SLet(s_prog,e) -> (* reutrn the original env *)
        let local_env = st_to_env (Some env) s_prog.symtab in 
        let local_env1 = List.fold_left exec_decl local_env s_prog.decls in
        show_env local_env1; let v,local_env2 = (eval local_env1 e) in v,env

        (*
    | Sast.SLet(dl, e) -> 
        let new_env = (List.fold_left 
                (fun env' dec -> match dec with
                      Vardef(str, e) -> let v, env = eval env e in 
                      {parent=env'.parent; 
                       ids=NameMap.add str {v_value=v;v_expr=None} env'.ids}
                    | _ -> interp_error 
                    ("Declaration in let binding must be a variable definition"))
                {parent=Some env; ids=NameMap.empty} dl) in 
            show_env new_env; 
            let v, new_env = (eval new_env e) in
                v, env (* reutrn the original env *)
                *)
        

(* exec_decl : env -> decl -> env' *)
(* execute the top-level declaration, in the global enviroment, 
 * return the updated global environment *)
(* environment -> Sast.s_dec -> environment' *)
and exec_decl env = function
    (*
      Sast.STypesig(sid) -> (* signature will generate a new fun *)
        (let vfun = VFun(sid.name,s_id,[]) in update_env env str vfun)
    | Sast.SFuncdec(f_decl) -> (* fun decl will be added to current *)
        (match NameMap.mem f_decl.fname env.ids with
              true -> (match (NameMap.find f_decl.fname env.ids) with 
                        | {v_value=VFun(name,fsig,def)} -> 
                            let vfun = VFun(name, fsig, f_decl::def) in update_env env name vfun
                        | _ -> interp_error("Not defined as a signature"))
            | false -> interp_error ("Function definition without a signature"))
    *)
    | Sast.SVardef(sid,se) -> 
        let v,env' = eval env se in
            update_env env' sid.name v
    | Sast.SMain(e) -> trace "exec smain" 
        (let v, env = eval env e in 
            write_to_file "testout.csv" v; env)
    | _ -> env


let solve_main symtab =
    let env = (st_to_env None symtab) in
    let v = resolve_name env "main"
    in print_string (string_of_value v ^ "\n")


(* run : program -> () *)
(* run the program *)
let run program s_prog = 

let decls = program in 
let globalE = {parent = None; 
        ids = List.fold_left (fun mp lst -> 
        NameMap.add lst.name {v_value=VUnknown;v_expr=None} mp) 
        NameMap.empty s_prog.symtab.identifiers}
in let _ = show_env globalE in

(* top-level declarations always run in global environment *)
List.fold_left exec_decl globalE decls

